module KripkeModel exposing (..)


type alias KripkeModel =
    { worlds : List World
    , relations : List Relation
    , evaluations : List Evaluation
    }


type alias World =
    Int


type alias Prop =
    Int


type alias Relation =
    { agentName : String
    , worldRelations : List (List World)
    }


type alias Evaluation =
    { world : World
    , propositions : List Prop
    }
