module Pages.KripkeModel exposing (..)

import Html exposing (Html)
import Debug exposing (todo)

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

type alias KripkeModel = 
    { worlds : List World
    , relations : List Relation
    , valuations : List Valuation
    }

-- MSG
type Msg
    = AddWorld World
    | AddRelation Relation
    | AddValuation Valuation

-- UPDATE
update : Msg -> KripkeModel -> KripkeModel
update msg model =
    case msg of
        AddWorld world ->
            { model | worlds = world :: model.worlds }
        AddRelation relation ->
            { model | relations = relation :: model.relations }
        AddValuation valuation ->
            { model | valuations = valuation :: model.valuations }

-- VIEW
view : KripkeModel -> Html Msg
view = 
    todo "Take input from user"