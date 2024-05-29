module Api exposing (..)

import Http
import Model exposing (Model, newModelEncoder)


fetchedReadMe : (Result Http.Error String -> msg) -> Cmd msg
fetchedReadMe onResponse =
    Http.get
        { url = "https://raw.githubusercontent.com/elm/browser/master/README.md"
        , expect = Http.expectString onResponse
        }


postModel : Model -> (Result Http.Error String -> msg) -> Cmd msg
postModel model onResponse =
    Http.request
        { method = "POST"
        , headers = []
        , url = "http://127.0.0.1:3000/model"
        , body = Http.jsonBody (newModelEncoder model)
        , expect = Http.expectString onResponse
        , timeout = Nothing
        , tracker = Nothing
        }
