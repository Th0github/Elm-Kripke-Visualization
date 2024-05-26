module Main exposing (..)

import Browser
import Html exposing (Html, div, input, text, br, button, h1)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (placeholder, value, class)
import Json.Encode exposing (encode, int, list, object, string)
import List.Extra
import Http
import Markdown exposing (toHtml) 
import Html.Lazy exposing (lazy)

-- MODEL
type alias Model =
    { worlds : List (Int, List Int) -- Each world has a list of propositions
    , agents : List String
    , relations : List (String, List (List Int)) -- Each agent has a list of world relations
    , jsonOutput : String
    , worldInput : String
    , agentInput : String
    , propositionInputs : List String -- Proposition inputs for each world
    , relationInputs : List (List Int) -- Relation inputs for each agent 2d list to map agent index to relations
    , readMeContent : String -- Readme content
    , showPopup : Bool -- Show the popup
    , showReadMe : Bool -- Show the Readme content
    }

init : () -> (Model, Cmd Msg)
init _ =
    ({ worlds = []
    , agents = []
    , relations = []
    , jsonOutput = ""
    , worldInput = ""
    , agentInput = ""
    , propositionInputs = []
    , relationInputs = []
    , readMeContent = ""
    , showPopup = False
    , showReadMe = False
    }, Cmd.none)

-- UPDATE

type Msg
    = UpdateWorldInput String
    | AddWorld
    | UpdateAgentInput String
    | AddAgent
    | UpdatePropositionInput Int String
    | AddProposition Int
    | UpdateRelationInput Int String
    | AddRelation Int
    | RecieveReadMe (Result Http.Error String)
    | ToogleReadMe 
    | FetchReadMe

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UpdateWorldInput input ->
            ({ model | worldInput = input }, Cmd.none)

        AddWorld ->
            let
                world = String.toInt model.worldInput |> Maybe.withDefault 0
                updatedWorlds = model.worlds ++ [( world, [] )]
            in
            ({ model | worlds = updatedWorlds, worldInput = "", propositionInputs = model.propositionInputs ++ [""], jsonOutput = toJson { model | worlds = updatedWorlds }}, Cmd.none)

        UpdateAgentInput input ->
            ({ model | agentInput = input }, Cmd.none)

        -- Adds the current agent input to the agents list
        AddAgent ->
            ({ model
            | agents = model.agents ++ [model.agentInput],
            relationInputs = model.relationInputs ++ [[]]
            , relations = model.relations ++ [(model.agentInput, [])]
            , agentInput = ""
            , jsonOutput = toJson { model | agents = model.agents ++ [model.agentInput] }
            }, Cmd.none)

        -- Updates the current proposition input for the given world index
        UpdatePropositionInput index input ->
            let
                updatedPropositions = List.Extra.updateAt index (\_ -> input) model.propositionInputs
            in
            ({ model | propositionInputs = updatedPropositions }, Cmd.none)

        -- Adds the current proposition input to the propositions list for the given world index from the propositionInputs list
        AddProposition index ->
            let
                proposition = String.toInt (List.Extra.getAt index model.propositionInputs |> Maybe.withDefault "") |> Maybe.withDefault 0
                updatedWorlds = List.indexedMap (\i (w, ps) -> if i == index then (w, ps ++ [proposition]) else (w, ps)) model.worlds
            in
            ({ model | worlds = updatedWorlds, propositionInputs = List.indexedMap (\i p -> if i == index then "" else p) model.propositionInputs, jsonOutput = toJson { model |worlds = updatedWorlds} }, Cmd.none)

        --Updates the current relation input for the given agent index
        UpdateRelationInput index input ->
            let
                _ = Debug.log "Input index" index
                _ = Debug.log "Input value" input
                inputAsList = String.words input |> List.map (\n -> String.toInt n |> Maybe.withDefault 0)
                _ = Debug.log "Input as list" inputAsList
                updatedRelationInputs = List.Extra.updateAt index (\_ -> inputAsList) model.relationInputs
                _ = Debug.log "Updated relation inputs" updatedRelationInputs
            in
            ({ model | relationInputs = updatedRelationInputs }, Cmd.none)


        -- Adds the current relation input to the relations list for the given agent index from the relationInputs list
        AddRelation agentIndex ->
            let
                maybeCurrentRelations = List.Extra.getAt agentIndex model.relationInputs
                currentRelations = Maybe.withDefault [] maybeCurrentRelations

                updateRelations (name, existingRelations) = (name, existingRelations ++ [currentRelations])

                _ = Debug.log "Current relations" currentRelations
                _ = Debug.log "Updated relations" (updateRelations (List.Extra.getAt agentIndex model.relations |> Maybe.withDefault ("", [])))

                -- Update the relations for the found agent index
                updatedRelations = List.Extra.updateAt agentIndex updateRelations model.relations

                in
                ({ model
                | relations = updatedRelations
                , relationInputs = List.indexedMap (\i r -> if i == agentIndex then [] else r) model.relationInputs

                , jsonOutput = toJson { model | relations = updatedRelations }
                }, Cmd.none)
        
        FetchReadMe ->
            (model, fetchedReadMe)
        
        RecieveReadMe (Ok content) ->
            ({ model | readMeContent = content }, Cmd.none)
        
        RecieveReadMe (Err _) ->
            ({model | readMeContent = "Failed to fetch Readme content"}, Cmd.none)

        ToogleReadMe ->
            ({model | showReadMe = not model.showReadMe}, Cmd.none)


-- VIEW
--<link rel="stylesheet" type="text/css" href="styles.css">
--Need to add this to the index.html file to link the css file
view : Model -> Html Msg
view model =
    div [ class "container-flex"]
        [ div [ class "left-column"]
            [ div [class "container"] [text "Kripke Model Creator"],
            input [class "input",  placeholder "Enter world (integer)", onInput UpdateWorldInput, value model.worldInput ] []
            , button [class "button", onClick AddWorld ] [ text "Add World" ]
            , br [] []
            , div [class "container"] (List.indexedMap (worldInputView model) model.worlds)
            , input [class "input",  placeholder "Enter agent name", onInput UpdateAgentInput, value model.agentInput ] []
            , button [class "button", onClick AddAgent ] [ text "Add Agent" ]
            , br [] []
            , div [class "container"] (List.indexedMap (agentInputView) model.agents)
            , button [class "button", onClick ToogleReadMe ] [ text "Toggle README/JSON" ]
            , button [class "button", onClick FetchReadMe ] [ text "Fetch README" ]
            ]-- , div [class "container"] [ text "Readme:", br [] [], text model.readMeContent ] -- have html render on writeside
        , div [ class "right-column" ]
            [ if model.showReadMe then
                div []
                    [ h1 [] [ text "REPORT" ]
                    , div [] [ lazy (Markdown.toHtml []) model.readMeContent ]
                    ]
              else
                div [class "container"]
                    [ text "Current JSON Output:", br [] [], text model.jsonOutput ]
            ]
        ]

worldInputView : Model -> Int -> (Int, List Int) -> Html Msg
worldInputView model index (world, propositions) =
    div [ class "container" ]
        [ text <| "World " ++ String.fromInt world ++ ": "
        , input [class "input",  placeholder "Add proposition (integer)", onInput (UpdatePropositionInput index), value (List.Extra.getAt index model.propositionInputs |> Maybe.withDefault "") ] []
        , button [class "button", onClick (AddProposition index) ] [ text "Add Proposition" ]
        , text <| " Propositions: " ++ String.join ", " (List.map String.fromInt propositions)
        , br [] []
        ]

agentInputView : Int -> String -> Html Msg
agentInputView index agent =
    div [ class "container" ]
        [ text <| "Agent " ++ agent ++ ": "
          ,  input
                [class "input",  placeholder "Add relation (list of worlds)"
                , onInput (UpdateRelationInput index)
                -- , value (List.Extra.getAt index model.re)
                ]
                []
        , button [class "button",  onClick (AddRelation index) ] [ text "Add Relation" ]
        , br [] []
        ]


-- JSON ENCODING

toJson : Model -> String
toJson model =
    object
        [ ("worlds", list (\(w, _) -> int w) model.worlds) ,
          ("valuations", list (\(w, ps) -> object [("world", int w), ("propositions", list int ps)]) model.worlds)
        -- , ("agents", list string model.agents)
        , ("relations", list (\(a, rs) -> object [("agentName", string a), ("worldRelations", list (list int) rs)]) model.relations)
        ]
        |> encode 4

-- Fetch Readme content
fetchedReadMe : Cmd Msg
fetchedReadMe =
    Http.get
        { url = "https://raw.githubusercontent.com/elm/browser/master/README.md"
        , expect = Http.expectString RecieveReadMe
        }

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- PROGRAM

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
