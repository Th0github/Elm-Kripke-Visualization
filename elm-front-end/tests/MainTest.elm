module MainTest exposing (..)

import Expect exposing (Expectation, equal, fail)
import Json.Decode exposing (decodeString)
import Json.Encode
import Main exposing (..)
import Model exposing (Model)
import Test exposing (..)



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
    , relationInputs = []
    , readMeContent = ""
    , showPopup = False
    , showReadMe = False
    }



-- Tests for update functions


testUpdateWorldInput : Test
testUpdateWorldInput =
    let
        ( updatedModel, _ ) =
            update (UpdateWorldInput "1") initialModel
    in
    Test.describe "UpdateWorldInput"
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
    Test.describe "AddWorld"
        [ Test.test "Adds a world correctly" <|
            \() -> Expect.equal updatedModel.worlds [ ( 1, [] ) ]
        ]


testUpdateAgentInput : Test
testUpdateAgentInput =
    let
        ( updatedModel, _ ) =
            update (UpdateAgentInput "Alice") initialModel
    in
    Test.describe "UpdateAgentInput"
        [ Test.test "Updates agent input correctly" <|
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
    Test.describe "AddAgent"
        [ Test.test "Adds an agent correctly" <|
            \() -> Expect.equal updatedModel.agents [ "Alice" ]
        ]


suite : Test
suite =
    Test.describe "Main"
        [ testUpdateWorldInput
        , testAddWorld
        , testUpdateAgentInput
        , testAddAgent
        ]
