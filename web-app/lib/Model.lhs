\section{Model}\label{sec:Model}
This section describes the Model

\begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Model ((!), (?), isTrue, (|=), kw, (-->), trueIn, Prop, Agent, Form, World, Relations, Valuation, Model (..)) where

import Control.Applicative ()
import Data.Aeson
import Data.Maybe

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

(!) :: Eq a => [(a,b)] -> a -> b
(!) v x = fromJust (lookup x v)

(?) :: Eq a => [[a]] -> a -> [a]
(?) lls x = head (filter (x `elem`) lls)

isTrue :: (Model, World) -> Form -> Bool
isTrue _ Top = True
isTrue (m,w) (P p) = p `elem ` (val m ! w)
isTrue (m,w) (Neg f) = not (isTrue (m,w) f)
isTrue (m,w) (Con f g) = isTrue (m,w) f && isTrue (m,w) g
isTrue (m,w) (K i f) = and [ isTrue (m,w') f | w' <- (rel m ! i) ? w ]
isTrue (m,w) (Ann f g) = isTrue (m,w) f <= isTrue (announce m f, w) g

(|=) :: (Model ,World) -> Form -> Bool
(|=) = isTrue

announce :: Model -> Form -> Model
announce m@(Mo ws r v) f = Mo newWorlds newRel newVal where
    newWorlds = [ w' | w' <- ws , isTrue (m,w') f ]
    newRel = [ (i, relFilter er) | (i,er) <- r ] where
        relFilter = filter (not . null) . map (filter (`elem ` newWorlds))  
    newVal = filter ((`elem ` newWorlds) . fst) v

(-->) :: Form -> Form -> Form
(-->) f = Con (Neg f) 

dis :: Form -> Form -> Form
dis f g = Neg (Con (Neg f) (Neg g))

kw :: Agent -> Form -> Form
kw i f = dis (K i f) (K i (Neg f))

trueIn :: Model -> Form -> [World]
trueIn (Mo u v r) f = filter (\w -> isTrue (Mo u v r, w) f) u

instance FromJSON Model where
  parseJSON (Object v) =
    Mo
      <$> v .: "worlds"
      <*> (v .: "relations" >>= mapM parseRel)
      <*> (v .: "valuations" >>= mapM parseVal)
    where
      parseRel (agent, worlds') = (,) agent <$> parseJSON (Array worlds')
      parseVal (world, props) = (,) world <$> parseJSON props
  parseJSON _ = fail "Could not parse Model from JSON"

instance ToJSON Model where
  toJSON (Mo worlds' rel' val') =
    object
      [ "worlds" .= worlds',
        "relations" .= map (\(a, ws) -> object ["agentName" .= a, "relations" .= ws]) rel',
        "valuations" .= map (\(w, ps) -> object ["world" .= w, "propositions" .= ps]) val'
      ]
\end{code}