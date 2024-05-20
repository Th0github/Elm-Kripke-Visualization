module Pages.ReadMe exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markdown exposing (toHtml) 
import Html.Lazy exposing (lazy)
import Json.Decode as Decode
import Http

type alias Model =
    { content : String }

type Msg
    = FetchReadMe
    | ReceiveReadMe (Result Http.Error String)
    | NoOp

fetchedReadMe : Cmd Msg
fetchedReadMe =
    Http.get
        { url = "https://raw.githubusercontent.com/Th0github/Elm-Kripke-Visualization/main/README.md?token=GHSAT0AAAAAACSE6BIFT2P2O5XPCIXEF4IMZSLU3FA" -- change 
        , expect = Http.expectString ReceiveReadMe
        }

-- UPDATE
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        FetchReadMe ->
            ( model, fetchedReadMe )

        ReceiveReadMe (Ok content) ->
            ( { model | content = content }, Cmd.none )

        ReceiveReadMe (Err _) ->
            ( { model | content = "Failed to load README.md" }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

-- VIEW
view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "README" ]
        , div [] [ lazy (toHtml []) model.content ]
        ]