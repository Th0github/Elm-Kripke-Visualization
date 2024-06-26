{-# LANGUAGE OverloadedStrings #-}

module Form where

import Data.Aeson
import qualified Data.Aeson.KeyMap as HM
import Data.Maybe
import Model

test :: IO ()
test = do
  -- Read the JSON response from a file or from the network response
  let jsonText = "{\"con\":[{\"p\":1},{\"neg\":{\"p\":2}}]}"
  -- Decode the JSON into the Form data type
  case decode jsonText of
    Just form -> putStrLn $ "Decoded formula: " ++ show (form :: Form)
    Nothing -> putStrLn "Failed to decode JSON"

atLeastOne :: Form
atLeastOne = dis (P 1) (dis (P 2) (P 3))

nobodyKnowsOwn :: Form
nobodyKnowsOwn = conSet [Neg (K "1" (P 1)), Neg (K "2" (P 2)), Neg (K "3" (P 3))]

everyoneKnowsAll :: Form
everyoneKnowsAll = conSet [kw "1" (P 1), kw "2" (P 2), kw "3" (P 3)]

instance ToJSON Form where
  toJSON Top = object ["top" .= True]
  toJSON (P p) = object ["p" .= p]
  toJSON (Neg f) = object ["neg" .= toJSON f]
  toJSON (Con f g) = object ["con" .= toJSON [f, g]]
  toJSON (K i f) = object ["knows" .= object ["agent" .= i, "formula" .= f]]
  toJSON (Ann f g) = object ["announce" .= object ["formula" .= f, "result" .= g]]

instance FromJSON Form where
  parseJSON = withObject "form" $ \o -> do
    let parseTop = return Top
        parseP = P <$> o .: "p"
        parseNeg = Neg <$> (o .: "neg" >>= parseJSON)
        parseCon = do
          subformulas <- o .: "con"
          case subformulas of
            [f, g] -> Con <$> parseJSON f <*> parseJSON g
            _ -> fail "Con must contain exactly two subformulas"
        parseK = K <$> o .: "agent" <*> (o .: "formula" >>= parseJSON)
        parseAnn = Ann <$> o .: "announce" <*> (o .: "formula" >>= parseJSON)

    case HM.keys o of
      ["top"] -> parseTop
      ["p"] -> parseP
      ["neg"] -> parseNeg
      ["con"] -> parseCon
      ["k"] -> parseK
      ["ann"] -> parseAnn
      _ -> fail "Unknown form type"

data Form = Top | P Prop | Neg Form | Con Form Form | K Agent Form | Ann Form Form
  deriving (Eq, Ord, Show)

(!) :: (Eq a) => [(a, b)] -> a -> b
(!) v x = fromJust (lookup x v)

(?) :: (Eq a) => [[a]] -> a -> [a]
(?) lls x = head (filter (x `elem`) lls)

isTrue :: (Model, World) -> Form -> Bool
isTrue _ Top = True
isTrue (m, w) (P p) = p `elem` (val m ! w)
isTrue (m, w) (Neg f) = not (isTrue (m, w) f)
isTrue (m, w) (Con f g) = isTrue (m, w) f && isTrue (m, w) g
isTrue (m, w) (K i f) = and [isTrue (m, w') f | w' <- (rel m ! i) ? w]
isTrue (m, w) (Ann f g) = isTrue (m, w) f <= isTrue (announce m f, w) g

(|=) :: (Model, World) -> Form -> Bool
(|=) = isTrue

announce :: Model -> Form -> Model
announce m@(Mo ws r v) f = Mo newWorlds newRel newVal
  where
    newWorlds = [w' | w' <- ws, isTrue (m, w') f]
    newRel = [(i, relFilter er) | (i, er) <- r]
      where
        relFilter = filter (not . null) . map (filter (`elem` newWorlds))
    newVal = filter ((`elem` newWorlds) . fst) v

(-->) :: Form -> Form -> Form
(-->) f = Con (Neg f)

conSet :: [Form] -> Form
conSet = foldr1 Con

dis :: Form -> Form -> Form
dis f g = Neg (Con (Neg f) (Neg g))

kw :: Agent -> Form -> Form
kw i f = dis (K i f) (K i (Neg f))

trueIn :: Form -> Model -> [World]
trueIn f m = [w | w <- worlds m, (m, w) |= f]

falseIn :: Form -> Model -> [World]
falseIn = trueIn . Neg