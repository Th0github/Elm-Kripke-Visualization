{-# LANGUAGE OverloadedStrings #-}

module Model (Prop, Agent, Form, World, Relations, Valuation, Model (..)) where

import Control.Applicative ()
import Data.Aeson

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
