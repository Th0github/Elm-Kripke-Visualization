--  \section{Model}\label{sec:Model}
--  This section describes the Model
--  \begin{code}
{-# LANGUAGE OverloadedStrings #-}

module Model where

import Data.Aeson

-- Define the Model
type Prop = Int

type Agent = String

type World = Int

type Relations = [(Agent, [[World]])]

type Valuation = [(World, [Prop])]

data Model = Mo
  { worlds :: [World],
    rel :: Relations,
    val :: Valuation
  }
  deriving (Eq, Ord, Show)

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

instance FromJSON Model where
  parseJSON =
    withObject
      "Model"
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

-- \end{code}
