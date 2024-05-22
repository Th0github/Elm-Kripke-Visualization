{-# LANGUAGE OverloadedStrings #-}

module Model (Prop, Agent, Form, World, Relations, Valuation, Model(..)) where

import Data.Aeson
import Control.Applicative()

-- Define the Model
type Prop = Int
type Agent = String

data Form = Top | P Prop | Neg Form | Con Form Form | K Agent Form | Ann Form Form
  deriving (Eq,Ord,Show)

type World = Int
type Relations = [(Agent, [[World]])]
type Valuation = [(World, [Prop])]
data Model = Mo { worlds :: [World]
                , rel :: Relations
                , val :: Valuation }
  deriving (Eq,Ord,Show)

instance FromJSON Model where
    parseJSON (Object v) = Mo <$>
                           v .: "worlds" <*>
                           (v .: "relations" >>= mapM parseRel) <*>
                           (v .: "valuations" >>= mapM parseVal) 
        where
        parseRel (agent, worlds') = (,) agent <$> parseJSON (Array worlds')
        parseVal (world, props) = (,) world <$> parseJSON props
    parseJSON _ = fail "Could not parse Model from JSON"

instance ToJSON Model where
    toJSON (Mo worlds' rel' val') =
        object ["worlds" .= worlds',
                "relations" .= map (\(a, ws) -> object ["agentName" .= a, "relations" .= ws]) rel',
                "valuations" .= map (\(w, ps) -> object ["world" .= w, "propositions" .= ps]) val']
