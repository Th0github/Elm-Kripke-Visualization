module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, div, text, a, Attribute)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Url exposing (Url)
import Debug exposing (todo)
import Browser exposing (UrlRequest)

type alias Model =
    { key : Nav.Key
    , page : Page
    }

type Page
    = KripkeModel
    | ReadMe

type Msg
    = LinkClicked Url
    | UrlChanged Url

main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = onUrlRequest
    , onUrlChange = onUrlChange
    }

onUrlRequest : UrlRequest -> Msg
onUrlRequest = todo "onUrlRequest"

onUrlChange : Url -> Msg
onUrlChange = todo "onUrlChange"

init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key, page = parseUrl url }
    , Cmd.none
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked url ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        UrlChanged url ->
            ( { model | page = parseUrl url }, Cmd.none )

view : Model -> Browser.Document Msg
view model =
    { title = "Navigation"
    , body = [ viewPage model.page ]
    }

viewPage : Page -> Html Msg
viewPage page =
    div []
        [ nav
        , case page of
            KripkeModel ->
                viewKripkeModel

            ReadMe ->
                viewReadMe
        ]

nav : Html Msg
nav = todo "nav"

-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

-- VIEW
viewKripkeModel : Html msg
viewKripkeModel =
    div [] [ text "This is the Kripke Model page." ]

viewReadMe : Html msg
viewReadMe =
    div [] [ text "This is the Read Me page." ]

parseUrl : Url -> Page
parseUrl url =
    case url.fragment of
        Just "kripkemodel" ->
            KripkeModel

        Just "readme" ->
            ReadMe

        _ ->
            KripkeModel