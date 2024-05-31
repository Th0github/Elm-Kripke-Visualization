module MainTest exposing (..)

import Expect
import Main exposing (..)
import Model exposing (Model)
import Test exposing (Test,describe,test)



-- Sample initial model for testing


initialModel : Model
initialModel =
    { worlds = []
      , agents = []
      , relations = []
      , jsonOutput = ""
      , worldInput = ""
      , agentInput = ""
      , propositionInputs = []
      , readMeContent = ""
      , valuationPropositionInput = ""
      , showPopup = False
      , showReadMe = False
      , showGraph = False
      , error = Nothing
      , currentRelationInputs = []
      , successMsg = ""
      }



-- Tests for update functions


testUpdateWorldInput : Test
testUpdateWorldInput =
    let
        ( updatedModel, _ ) =
            update (UpdateWorldInput "1") initialModel
    in
    describe "UpdateWorldInput"
        [ Test.test "Updates world input correctly" <|
            \() -> Expect.equal updatedModel.worldInput "1"
        ]


testAddWorld : Test
testAddWorld =
    let
        modelWithWorldInput =
            { initialModel | worldInput = "1" }

        ( updatedModel, _ ) =
            update AddWorld modelWithWorldInput
    in
    describe "AddWorld"
        [ Test.test "Adds a world correctly" <|
            \() -> Expect.equal updatedModel.worlds [ ( 1, [] ) ]
        ]


testUpdateAgentInput : Test
testUpdateAgentInput =
    let
        ( updatedModel, _ ) =
            update (UpdateAgentInput "Alice") initialModel
    in
        describe "UpdateAgentInput"
        [ test "Updates agent input correctly" <|
            \() -> Expect.equal updatedModel.agentInput "Alice"
        ]


testAddAgent : Test
testAddAgent =
    let
        modelWithAgentInput =
            { initialModel | agentInput = "Alice" }

        ( updatedModel, _ ) =
            update AddAgent modelWithAgentInput
    in
    describe "AddAgent"
        [ test "Adds an agent correctly" <|
            \() -> Expect.equal updatedModel.agents [ "Alice" ]
        ]


suite : Test
suite =
    describe "Main"
        [ testUpdateWorldInput
        , testAddWorld
        , testUpdateAgentInput
        , testAddAgent
        ]
