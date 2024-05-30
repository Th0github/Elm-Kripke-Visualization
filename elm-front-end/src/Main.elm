module Main exposing (..)

import Browser
import Html exposing (Html, br, button, div, h1, input, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Html.Lazy exposing (lazy)
import Http
import Json.Decode exposing (decodeValue)
import Json.Encode exposing (encode, int, list, object, string)
import List.Extra
import Markdown exposing (toHtml)
import Model exposing (Model, newModelEncoder)



-- MODEL
-- The model is initialized
init : () -> ( Model, Cmd Msg )
init _ =
    ( { worlds = []
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
      , error = Nothing
      }
    , Cmd.none
    )



-- UPDATE
-- Here, are the actions that can be performed on the model, which updates the UI
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
    | ToggleReadMe
    | FetchReadMe
    | PostKripkeModel
    | RemoveWorld Int
    | PostedKripkeModel (Result Http.Error String)
    | GotKripkeModel (Result Http.Error String)


-- The update function takes a message and a model and returns a new model and a command.
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateWorldInput input ->
            ( { model | worldInput = input }, Cmd.none )
        -- Adds the current world input to the worlds list
        AddWorld ->
            let
                maybeWorld =
                    String.toInt model.worldInput
                (updatedWorlds, errorMsg) =
                    case maybeWorld of
                        Just world ->
                            let
                                worldExists =
                                    List.any (\(w, _) -> w == world) model.worlds
                            in
                            if worldExists then
                                (model.worlds, Just "Error: World already exists")
                            else
                                (model.worlds ++ [ ( world, [] ) ], Nothing)

                        Nothing ->
                            (model.worlds, Just "Error: Invalid input")


            in
            ( { model
                | worlds = updatedWorlds
                , worldInput = ""
                , propositionInputs = model.propositionInputs ++ [ "" ]
                , jsonOutput = toJson { model | worlds = updatedWorlds }
                , error = errorMsg
            }
            , Cmd.none
            )

        RemoveWorld index ->
            let
                updatedWorlds = List.Extra.removeAt index model.worlds
            in
            ( { model | worlds = updatedWorlds, jsonOutput = {
                model | worlds = updatedWorlds
            } |> toJson
            }, Cmd.none )

        UpdateAgentInput input ->
            ( { model | agentInput = input }, Cmd.none )

        -- Adds the current agent input to the agents list
        AddAgent ->
            ( { model
                | agents = model.agents ++ [ model.agentInput ]
                , relationInputs = model.relationInputs ++ [ [] ]
                , relations = model.relations ++ [ ( model.agentInput, [] ) ]
                , agentInput = ""
                , jsonOutput = toJson { model | agents = model.agents ++ [ model.agentInput ] }
              }
            , Cmd.none
            )

        -- Updates the current proposition input for the given world index
        UpdatePropositionInput index input ->
            let
                updatedPropositions =
                    List.Extra.updateAt index (\_ -> input) model.propositionInputs
            in
            ( { model | propositionInputs = updatedPropositions }, Cmd.none )

        -- Adds the current proposition input to the propositions list for the given world index from the propositionInputs list
        AddProposition index ->
            let
                maybeProposition =
                    List.Extra.getAt index model.propositionInputs
                        |> Maybe.andThen String.toInt

                (updatedWorlds, errorMsg) =
                    case maybeProposition of
                        Just proposition ->
                            let
                                propositionExists =
                                    case List.Extra.getAt index model.worlds of
                                        Just ( _, ps ) -> List.any (\p -> p == proposition) ps
                                        Nothing -> False

                                updates =
                                    if propositionExists then
                                        model.worlds
                                    else
                                        List.indexedMap
                                            (\i ( w, ps ) ->
                                                if i == index && not propositionExists then
                                                    ( w, ps ++ [ proposition ] )
                                                else
                                                    ( w, ps )
                                            )
                                            model.worlds
                            in
                            if propositionExists then
                                (updates, Just "Error: Proposition already exists")
                            else
                                (updates, Nothing)

                        Nothing ->
                            (model.worlds, Just "Error: Invalid input")
            in
            ( { model
                | worlds = updatedWorlds
                , propositionInputs =
                    List.indexedMap
                        (\i p ->
                            if i == index then
                                ""
                            else
                                p
                        )
                        model.propositionInputs
                , jsonOutput = toJson { model | worlds = updatedWorlds }
                , error = errorMsg
            }
            , Cmd.none
            )
        --Updates the current relation input for the given agent index
        UpdateRelationInput index input ->
            let

                inputAsList =
                    String.words input |> List.map (\n -> String.toInt n |> Maybe.withDefault 0)

                updatedRelationInputs =
                    List.Extra.updateAt index (\_ -> inputAsList) model.relationInputs
            in
            ( { model | relationInputs = updatedRelationInputs }, Cmd.none )

        -- Adds the current relation input to the relations list for the given agent index from the relationInputs list
        AddRelation agentIndex ->
            let
                maybeCurrentRelations =
                    List.Extra.getAt agentIndex model.relationInputs

                currentRelations =
                    Maybe.withDefault [] maybeCurrentRelations

                updateRelations ( name, existingRelations ) =
                    ( name, existingRelations ++ [ currentRelations ] )

                -- Update the relations for the found agent index
                updatedRelations =
                    List.Extra.updateAt agentIndex updateRelations model.relations

            in
            ( { model
                | relations = updatedRelations
                , relationInputs =
                    List.indexedMap
                        (\i r ->
                            if i == agentIndex then
                                []

                            else
                                r
                        )
                        model.relationInputs
                , jsonOutput = toJson { model | relations = updatedRelations }
              }
            , Cmd.none
            )

        FetchReadMe ->
            ( model, fetchedReadMe )

        RecieveReadMe (Ok content) ->
            ( { model | readMeContent = content }, Cmd.none )

        RecieveReadMe (Err _) ->
            ( { model | readMeContent = "Failed to fetch Readme content" }, Cmd.none )

        ToggleReadMe ->
            ( { model | showReadMe = not model.showReadMe }, Cmd.none )

        PostKripkeModel ->
            Debug.log "Post" ( model, postModel model )

        -- let
        --     _ =
        --         Debug.log "Post"
        -- in
        -- ( model, postModel model )
        PostedKripkeModel _ ->
            ( model, Cmd.none )

        GotKripkeModel _ ->
            Debug.todo "TODO"



-- VIEW
-- The view functions defines the layout of the UI and how the model is displayed


view : Model -> Html Msg
view model =
    div [ class "container-flex" ]
        [ div [ class "left-column" ]
            [ div [ class "container" ] [ text "Kripke Model Creator" ]
            , input [ class "input", placeholder "Enter world (integer)", onInput UpdateWorldInput, value model.worldInput ] []
            , button [ class "button", onClick AddWorld ] [ text "Add World" ]
            , br [] []
            , div [ class "container" ] (List.indexedMap (worldInputView model) model.worlds)
            , input [ class "input", placeholder "Enter agent name", onInput UpdateAgentInput, value model.agentInput ] []
            , button [ class "button", onClick AddAgent ] [ text "Add Agent" ]
            , br [] []
            , viewError model.error
            , div [ class "container" ] (List.indexedMap agentInputView model.agents)
            , button [ class "button", onClick ToggleReadMe ] [ text "Toggle README/JSON" ]
            , button [ class "button", onClick FetchReadMe ] [ text "Fetch README" ]
            , button [ class "button", onClick PostKripkeModel ] [ text "Post Model" ]
            ]

        -- , div [class "container"] [ text "Readme:", br [] [], text model.readMeContent ] -- have html render on writeside
        , div [ class "right-column" ]
            [ if model.showReadMe then
                div []
                    [ h1 [] [ text "REPORT" ]
                    , div [] [ lazy (Markdown.toHtml []) model.readMeContent ]
                    ]

              else
                div [ class "container" ]
                    [ text "Current JSON Output:", br [] [], text model.jsonOutput ]
            ]
        ]


worldInputView : Model -> Int -> ( Int, List Int ) -> Html Msg
worldInputView model index ( world, propositions ) =
    div [ class "container" ]
        [ text <| "World " ++ String.fromInt world ++ ":   " , button [ class "button-secondary", onClick (RemoveWorld index) ] [ text "Remove" ]
        , input [ class "input", placeholder "Add proposition (integer)", onInput (UpdatePropositionInput index), value (List.Extra.getAt index model.propositionInputs |> Maybe.withDefault "") ] []
        , button [ class "button", onClick (AddProposition index) ] [ text "Add Proposition" ]
        , text <| " Propositions: " ++ String.join ", " (List.map String.fromInt propositions)
        , br [] []
        ]



agentInputView : Int -> String -> Html Msg
agentInputView index agent =
    div [ class "container" ]
        [ text <| "Agent " ++ agent ++ ": "
        , input
            [ class "input"
            , placeholder "Add relation (list of worlds)"
            , onInput (UpdateRelationInput index)

            -- , value (List.Extra.getAt index model.re)
            ]
            []
        , button [ class "button", onClick (AddRelation index) ] [ text "Add Relation" ]
        , br [] []
        ]



-- JSON ENCODING


toJson : Model -> String
toJson model =
    object
        [ ( "worlds", list (\( w, _ ) -> int w) model.worlds )
        , ( "valuations", list (\( w, ps ) -> object [ ( "world", int w ), ( "propositions", list int ps ) ]) model.worlds )

        -- , ("agents", list string model.agents)
        , ( "relations", list (\( a, rs ) -> object [ ( "agentName", string a ), ( "worldRelations", list (list int) rs ) ]) model.relations )
        ]
        |> encode 4


viewError : Maybe String -> Html msg
viewError maybeError =
    case maybeError of
        Just errorMsg ->
            div [ class "error" ] [ text errorMsg ]

        Nothing ->
            text ""


-- Fetch Readme content


fetchedReadMe : Cmd Msg
fetchedReadMe =
    Http.get
        { url = "https://raw.githubusercontent.com/elm/browser/master/README.md"
        , expect = Http.expectString RecieveReadMe
        }


postModel : Model -> Cmd Msg
postModel model =
    Http.request
        { method = "POST"
        , headers = []
        , url = "http://127.0.0.1:3000/model"
        , body = Http.jsonBody (newModelEncoder model)
        , expect = Http.expectString PostedKripkeModel
        , timeout = Nothing
        , tracker = Nothing
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
