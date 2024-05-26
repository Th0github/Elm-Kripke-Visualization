module Model exposing (Model, World, newModelEncoder)

import Json.Encode exposing (Value, int, list, object, string)


type alias World =
    Int


type alias Model =
    { worlds : List ( World, List Int ) -- Each world has a list of propositions
    , agents : List String
    , relations : List ( String, List (List Int) ) -- Each agent has a list of world relations
    , jsonOutput : String
    , worldInput : String
    , agentInput : String
    , propositionInputs : List String -- Proposition inputs for each world
    , relationInputs : List (List Int) -- Relation inputs for each agent 2d list to map agent index to relations
    , readMeContent : String -- Readme content
    , showPopup : Bool -- Show the popup
    , showReadMe : Bool -- Show the Readme content
    }


newModelEncoder : Model -> Value
newModelEncoder model =
    object
        [ ( "worlds", list (\( w, _ ) -> int w) model.worlds )
        , ( "valuations", list (\( w, ps ) -> object [ ( "world", int w ), ( "propositions", list int ps ) ]) model.worlds )

        -- , ("agents", list string model.agents)
        , ( "relations", list (\( a, rs ) -> object [ ( "agentName", string a ), ( "worldRelations", list (list int) rs ) ]) model.relations )
        ]