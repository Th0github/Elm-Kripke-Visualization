module Api exposing (..)

import Http
import Model exposing (Model, newModelEncoder, newModelPropositionEncoder)


fetchReadMe : (Result Http.Error String -> msg) -> Cmd msg
fetchReadMe onResponse =
    Http.get
        { url = "https://raw.githubusercontent.com/Th0github/Elm-Kripke-Visualization/main/elm-front-end/src/Markdowns/HELP.md"
        , expect = Http.expectString onResponse
        }


fetchElmStuff : (Result Http.Error String -> msg) -> Cmd msg
fetchElmStuff onResponse =
    Http.get
        { url = "https://raw.githubusercontent.com/Th0github/Elm-Kripke-Visualization/main/elm-front-end/src/Markdowns/ElmStuff.md"
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
    -- Http.post
    --     {
    --     body = Http.jsonBody (newModelEncoder model)
    --     , url = "http://127.0.0.1:3000/model"
    --     , expect = Http.expectString onResponse
    --     }

evaluateModel : Model -> (Result Http.Error String -> msg) -> Cmd msg
evaluateModel model onResponse =
  Http.request
    { method = "POST"
    , headers = []
    , url = "https://example.com/evaluate"
    , body = Http.jsonBody (newModelPropositionEncoder model)
    , expect = Http.expectString onResponse
    , timeout = Nothing
    , tracker = Nothing
    }
    -- Http.post
    --     {
    --     body = Http.jsonBody (newModelEncoder model)
    --     , url = "http://127.0.0.1:3000/evaluate"
    --     , expect = Http.expectString onResponse
    --     }
