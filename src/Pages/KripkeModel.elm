module Pages.KripkeModel exposing (..)

type alias World = Int
type alias Proposition = Int

type alias Relation = 
    { agentName : String
    , relation : List (List Int)
    }
type alias Valuation = 
    { world : World
    , propositions : List Proposition
    }

type KripkeModel = 
    KrM (List World) Valuation Relation
