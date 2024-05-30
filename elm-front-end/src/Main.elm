module Main exposing (..)

import Api exposing (fetchElmStuff, fetchReadMe, postModel)
import Browser
import GraphKripke exposing (getSvg)
import Html exposing (Html, br, button, div, h1, input, pre, span, text)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput, onMouseEnter)
import Html.Lazy exposing (lazy)
import Http
import Json.Encode exposing (encode, int, list, object, string)
import List.Extra
import Markdown
import Model exposing (Model)
import Error exposing (KMError, errorToString)
import KripkeModel exposing (KripkeModel)

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
      , readMeContent = ""
      , showPopup = False
      , showReadMe = False
      , error = Nothing
      , currentRelationInputs = []
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
    | ReceiveReadMe (Result Http.Error String)
    | ToggleReadMe
    | FetchReadMe
    | FetchElmStuffReadMe
    | PostKripkeModel
    | RemoveWorld Int
    | RemoveAgent Int
    | PostedKripkeModel (Result Http.Error String)
    | GotKripkeModel (Result Http.Error String)
    | ToggleAndFetch
    | ToggleChoiceBox




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

                ( updatedWorlds, errorMsg ) =
                    case maybeWorld of
                        Just world ->
                            let
                                worldExists =
                                    List.any (\( w, _ ) -> w == world) model.worlds
                            in
                            if worldExists then
                                (model.worlds, Just Error.WorldExists)
                            else
                                ( model.worlds ++ [ ( world, [] ) ], Nothing )

                        Nothing ->
                            (model.worlds, Just Error.InvalidInput)


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
                updatedWorlds =
                    List.Extra.removeAt index model.worlds
            in
            ( { model
                | worlds = updatedWorlds
                , jsonOutput =
                    { model
                        | worlds = updatedWorlds
                    }
                        |> toJson
              }
            , Cmd.none
            )

        UpdateAgentInput input ->
            ( { model | agentInput = input }, Cmd.none )

        -- Adds the current agent input to the agents list
        AddAgent ->
            let
                agentExists =
                    List.any (\a -> a == model.agentInput) model.agents

                updatedModel =
                    if agentExists then
                        { model | error = Just Error.AgentExists }
                    else
                        { model
                            | agents = model.agents ++ [ model.agentInput ]
                            , currentRelationInputs = model.currentRelationInputs ++ [""]
                            , relations = model.relations ++ [ ( model.agentInput, [] ) ]
                            , agentInput = ""
                            , jsonOutput = toJson { model | agents = model.agents ++ [ model.agentInput ], relations = model.relations ++ [ ( model.agentInput, [] ) ] }
                            , error = Nothing
                        }
            in
            (updatedModel, Cmd.none)
        RemoveAgent idx ->
            let
                updatedAgents =
                    List.Extra.removeAt idx model.agents
                updatedRelations =
                    List.Extra.removeAt idx model.relations
                updatedCurrentRelationInputs =
                    List.Extra.removeAt idx model.currentRelationInputs
            in
            ( { model | agents = updatedAgents, relations = updatedRelations, currentRelationInputs = updatedCurrentRelationInputs,
            jsonOutput = toJson { model | agents = updatedAgents, relations = updatedRelations}
            }, Cmd.none )

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

                ( updatedWorlds, errorMsg ) =
                    case maybeProposition of
                        Just proposition ->
                            let
                                propositionExists =
                                    case List.Extra.getAt index model.worlds of
                                        Just ( _, ps ) ->
                                            List.any (\p -> p == proposition) ps

                                        Nothing ->
                                            False

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
                                (updates, Just Error.PropositionExists)
                            else
                                ( updates, Nothing )

                        Nothing ->
                            (model.worlds, Just Error.InvalidInput)
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
                updatedCurrentRelationInputs =
                    List.Extra.updateAt index (\a -> input) model.currentRelationInputs
            in
            ( { model | currentRelationInputs = updatedCurrentRelationInputs }, Cmd.none )

        AddRelation agentIndex ->
            let
                maybeCurrentInput =
                    List.Extra.getAt agentIndex model.currentRelationInputs

                maybeAgentRelations =
                    List.Extra.getAt agentIndex model.relations
            in
            case (maybeCurrentInput, maybeAgentRelations) of
                (Just currentInput, Just (agentName, existingRelations)) ->
                    case parseInputToRelation currentInput of
                        Just parsedInput ->
                            let
                                relationExists =
                                    List.member parsedInput existingRelations

                                worldsExist =
                                    List.all (\w -> List.any (\(world, _) -> world == w) model.worlds) parsedInput

                                _ =
                                    Debug.log "Worlds Exist" worldsExist

                                updatedRelations =
                                    if relationExists then
                                        model.relations
                                    else
                                        List.Extra.updateAt agentIndex (\_ -> (agentName, existingRelations ++ [parsedInput])) model.relations

                                updatedCurrentRelationInputs =
                                    List.Extra.updateAt agentIndex (\a -> "") model.currentRelationInputs

                                updatedModel =
                                    if relationExists then
                                        { model | error = Just Error.RelationExists, currentRelationInputs = updatedCurrentRelationInputs }
                                    else if not worldsExist then
                                        { model | error = Just Error.WorldNotExists }
                                    else
                                        { model
                                            | relations = updatedRelations
                                            , currentRelationInputs = updatedCurrentRelationInputs
                                            , error = Nothing
                                            , jsonOutput = toJson { model | relations = updatedRelations }
                                        }
                            in
                            ( updatedModel, Cmd.none )

                        Nothing ->
                            ( { model | error = Just Error.InvalidRelationInput }, Cmd.none )

                (Nothing, _) ->
                    ( { model | error = Just Error.InvalidRelationInput }, Cmd.none )

                (_, Nothing) ->
                    ( { model | error = Just Error.AgentDoesNotExist }, Cmd.none )



        FetchReadMe ->
            ( model, fetchReadMe ReceiveReadMe )

        FetchElmStuffReadMe ->
            ( model, fetchElmStuff ReceiveReadMe )

        ReceiveReadMe (Ok content) ->
            ( { model | readMeContent = content }, Cmd.none )

        ReceiveReadMe (Err _) ->
            ( { model | readMeContent = "Failed to fetch Readme content" }, Cmd.none )

        ToggleReadMe ->
            ( { model | showReadMe = not model.showReadMe }, Cmd.none )

        PostKripkeModel ->
            Debug.log "Post" ( model, postModel model PostedKripkeModel )

        -- let
        --     _ =
        --         Debug.log "Post"
        -- in
        -- ( model, postModel model )
        PostedKripkeModel _ ->
            ( model, Cmd.none )

        GotKripkeModel _ ->
            Debug.todo "TODO"

        ToggleAndFetch ->
            let
                ( updatedModel, cmd1 ) =
                    update ToggleReadMe model

                ( finalModel, cmd2 ) =
                    update FetchReadMe updatedModel
            in
            ( finalModel, Cmd.batch [ cmd1, cmd2 ] )

        ToggleChoiceBox ->
            ( { model | showPopup = not model.showPopup }, Cmd.none )



-- VIEW
-- The view functions defines the layout of the UI and how the model is displayed


view : Model -> Html Msg
view model =
    div [ class "container-flex" ]
        [ div [ class "left-column" ]
            [ div [ class "head-container" ]
                [ text "Kripke Model Creator"
                , button [ class "button", onMouseEnter ToggleChoiceBox, onClick ToggleAndFetch ] [ text "Toggle Help" ]
                , if model.showPopup then
                    div []
                        [ button [ class "button", onClick FetchReadMe ] [ text "Help Page" ]
                        , button [ class "button", onClick FetchElmStuffReadMe ] [ text "Elm Stuff" ]
                        ]

                  else
                    text ""
                ]
            , viewError model.error
            , br [] []
            , text "Worlds"
            , br [] []
            , br [] []
            , input [ class "input", placeholder "Enter world (integer)", onInput UpdateWorldInput, value model.worldInput ] []
            , button [ class "button", onClick AddWorld ] [ text "Add World" ]
            , br [] []
            , div [ class "container" ] (List.indexedMap (worldInputView model) model.worlds)
            , text "Agents"
            , br [] []
            , br [] []
            , input [ class "input", placeholder "Enter agent name", onInput UpdateAgentInput, value model.agentInput ] []
            , button [ class "button", onClick AddAgent ] [ text "Add Agent" ]
            , br [] []
           , div [ class "container" ] (List.indexedMap (\index agent -> agentInputView index agent (List.Extra.getAt index model.currentRelationInputs |> Maybe.withDefault "")) model.agents)
            , button [ class "button", onClick PostKripkeModel ] [ text "Post Model" ]
            ]

        -- , div [class "container"] [ text "Readme:", br [] [], text model.readMeContent ] -- have html render on writeside
        , div [ class "right-column" ]
            [ if model.showReadMe then
                div []
                    [ h1 [] [ text "Documentation" ]
                    , div [] [ lazy (Markdown.toHtml []) model.readMeContent ]
                    ]

              else
                div [ class "container" ]
                    [ text "Current JSON Output:", br [] [], highlightJson model.jsonOutput, getSvg (modelToKripke model) ]
            ]
        ]



worldInputView : Model -> Int -> ( Int, List Int ) -> Html Msg
worldInputView model index ( world, propositions ) =
    div [ class "container" ]
        [ div [ class "world-header" ] [ text <| "World " ++ String.fromInt world ++ ":   ", button [ class "button-secondary", onClick (RemoveWorld index) ] [ text "X" ] ]
        , input [ class "input", placeholder "Add proposition (integer)", onInput (UpdatePropositionInput index), value (List.Extra.getAt index model.propositionInputs |> Maybe.withDefault "") ] []
        , button [ class "button", onClick (AddProposition index) ] [ text "Add Proposition" ]
        , br [] []
        ]

parseInputToRelation : String -> Maybe (List Int)
parseInputToRelation input =
    let
        isValidInteger s =
            case String.toInt s of
                Just _ -> True
                Nothing -> False

        inputAsList =
            String.words input

        allValid =
            List.all isValidInteger inputAsList

        parsedInputAsList =
            List.filterMap String.toInt inputAsList
    in
    if allValid then
        Just parsedInputAsList
    else
        Nothing



agentInputView : Int -> String -> String -> Html Msg
agentInputView index agentName currentValue =
    div [ class "container" ]
        [ div [ class "world-header" ]
            [ text <| "Agent " ++ agentName ++ ": "
            , button [ class "button-secondary", onClick (RemoveAgent index) ] [ text "X" ]
            ]
        , input
            [ class "input"
            , placeholder "Add relation (list of worlds, space separated integers)"
            , onInput (UpdateRelationInput index)
            , value currentValue
            ]
            []
        , button [ class "button", onClick (AddRelation index) ] [ text "Add Relation" ]
        , br [] []
        ]



-- JSON ENCODING

modelToKripke : Model -> KripkeModel
modelToKripke model =
    let
        worlds =
            List.map Tuple.first model.worlds

        evaluations =
            List.map (\(w, ps) -> { world = w, propositions = ps }) model.worlds

        relations =
            List.map (\(a, rs) -> { agentName = a, worldRelations = rs }) model.relations
    in
    { worlds = worlds
    , relations = relations
    , evaluations = evaluations
    }

toJson : Model -> String
toJson model =
    object
        [ ( "worlds", list (\( w, _ ) -> int w) model.worlds )
        , ( "valuations", list (\( w, ps ) -> object [ ( "world", int w ), ( "propositions", list int ps ) ]) model.worlds )

        -- , ("agents", list string model.agents)
        , ( "relations", list (\( a, rs ) -> object [ ( "agentName", string a ), ( "worldRelations", list (list int) rs ) ]) model.relations )
        ]
        |> encode 4


viewError : Maybe KMError -> Html msg
viewError maybeError =
    case maybeError of
        Just errorMsg ->
            div [ class "error" ] [ text (errorToString errorMsg)]
        Nothing ->
            text ""


highlightJson : String -> Html msg
highlightJson jsonString =
    let
        highlight : Char -> Html msg
        highlight c =
            let
                charStr =
                    String.fromChar c
            in
            case c of
                '{' ->
                    span [ class "brace" ] [ text charStr ]

                '}' ->
                    span [ class "brace" ] [ text charStr ]

                '[' ->
                    span [ class "brace" ] [ text charStr ]

                ']' ->
                    span [ class "brace" ] [ text charStr ]

                '"' ->
                    if String.contains ": " (String.dropLeft 1 jsonString) then
                        span [ class "key" ] [ text charStr ]

                    else
                        span [ class "string" ] [ text charStr ]

                ',' ->
                    text charStr

                ':' ->
                    text charStr

                ' ' ->
                    text charStr

                '-' ->
                    text charStr

                '.' ->
                    text charStr

                _ ->
                    if Char.isDigit c then
                        span [ class "number" ] [ text charStr ]

                    else if String.contains "truefalse" charStr then
                        span [ class "boolean" ] [ text charStr ]

                    else
                        text charStr
    in
    pre [ class "syntax-highlighted-json" ]
        (List.map highlight (String.toList jsonString))



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
