module Api exposing (..)

import Http
import Model exposing (Model, newModelEncoder)


fetchReadMe : (Result Http.Error String -> msg) -> Cmd msg
fetchReadMe onResponse =
    Http.get
        { url = "https://raw.githubusercontent.com/Th0github/Elm-Kripke-Visualization/style/hoover-button/elm-front-end/src/Markdowns/HELP.md"
        , expect = Http.expectString onResponse
        }


fetchElmStuff : (Result Http.Error String -> msg) -> Cmd msg
fetchElmStuff onResponse =
    Http.get
        { url = "https://raw.githubusercontent.com/Th0github/Elm-Kripke-Visualization/style/hoover-button/elm-front-end/src/Markdowns/ElmStuff.md"
        , expect = Http.expectString onResponse
        }


postModel : Model -> (Result Http.Error String -> msg) -> Cmd msg
postModel model onResponse =
    Http.post
        {
        body = Http.jsonBody (newModelEncoder model)
        , url = "http://127.0.0.1:3000/model"
        , expect = Http.expectString onResponse
        }

evaluateModel : Model -> (Result Http.Error String -> msg) -> Cmd msg
evaluateModel model onResponse =
    Http.post
        {
        body = Http.jsonBody (newModelEncoder model)
        , url = "http://127.0.0.1:3000/evaluate"
        , expect = Http.expectString onResponse
        }
