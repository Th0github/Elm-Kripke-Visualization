 \section{Model}\label{sec:Model}
 This section describes the Model
 \begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Model where

import Data.Aeson
import Data.Maybe
import qualified Data.Aeson.KeyMap as HM

-- Define the Model
type Prop = Int

type Agent = String

data Form = Top | P Prop | Neg Form | Con Form Form | K Agent Form | Ann Form Form
  deriving (Eq, Ord, Show)

type World = Int

type Relations = [(Agent, [[World]])]

type Valuation = [(World, [Prop])]

data Model = Mo
  { worlds :: [World],
    rel :: Relations,
    val :: Valuation
  }
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
trueIn f m = [ w | w <- worlds m, (m,w) |= f ]

falseIn :: Form -> Model -> [World]
falseIn = trueIn . Neg

muddyStart :: Model
muddyStart =
  Mo
    [0, 1, 2, 3, 4, 5, 6, 7]
    [ ("1", [[0, 4], [2, 6], [3, 7], [1, 5]]),
      ("2", [[0, 2], [4, 6], [5, 7], [1, 3]]),
      ("3", [[0, 1], [4, 5], [6, 7], [2, 3]])
    ]
    [ (0, []),
      (1, [3]),
      (2, [2]),
      (3, [2, 3]),
      (4, [1]),
      (5, [1, 3]),
      (6, [1, 2]),
      (7, [1, 2, 3])
    ]

atLeastOne :: Form
atLeastOne = dis (P 1) (dis (P 2) (P 3))

nobodyKnowsOwn :: Form
nobodyKnowsOwn = conSet [Neg (K "1" (P 1)), Neg (K "2" (P 2)), Neg (K "3" (P 3))]

everyoneKnowsAll :: Form
everyoneKnowsAll = conSet [kw "1" (P 1), kw "2" (P 2), kw "3" (P 3)]

test :: IO ()
test = do
  -- Read the JSON response from a file or from the network response
  let jsonText = "{\"con\":[{\"p\":1},{\"neg\":{\"p\":2}}]}"
  -- Decode the JSON into the Form data type
  case decode jsonText of
    Just form -> putStrLn $ "Decoded formula: " ++ show (form :: Form)
    Nothing -> putStrLn "Failed to decode JSON"

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

instance FromJSON Model where
  parseJSON =
    withObject
      "Object"
      ( \o ->
          do
            _worlds <- o .: "worlds"
            _relations <- o .: "relations" >>= mapM parseRel
            _valutations <- o .: "valuations" >>= mapM parseVal
            return (Mo _worlds _relations _valutations)
      )
    where
      parseRel = withObject "Relations" (\o' -> (,) <$> (o' .: "agentName") <*> (o' .: "worldRelations"))
      parseVal = withObject "Valuations" (\o' -> (,) <$> (o' .: "world") <*> (o' .: "propositions"))

instance ToJSON Model where
  toJSON (Mo worlds' rel' val') =
    object
      [ "worlds" .= worlds',
        "relations" .= map (\(a, ws) -> object ["agentName" .= a, "worldRelations" .= ws]) rel',
        "valuations" .= map (\(w, ps) -> object ["world" .= w, "propositions" .= ps]) val'
      ]

\end{code}
