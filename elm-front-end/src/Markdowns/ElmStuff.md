# Elm Stuff
To have reached this page, you have hovered over the toggle button, pressed a button request to a Markdown document, which Elm then converted to HTML to be displayed.

## How This Happens in Elm
A break down how this happened in Elm syntax is given below:
1. Hover Over Toggle Button:
*Model and Initial State:*
```{elm}
init =({-- Kripke model
      , readMeContent = ""
      , showPopup = False
      , showReadMe = False
      }, Cmd.none
    )
```    

*Message Types:*
```{elm}
type Msg = -- other Kripke types
    | RecieveReadMe (Result Http.Error String)
    | ToggleReadMe
    | FetchReadMe
    | ToggleAndFetch
    | ToggleChoiceBox
```
2. **Update** function handles user interaction and message handling:
```{elm}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
        FetchReadMe ->
            ( model, fetchedReadMe )

        FetchElmStuffReadMe ->
            ( model, fetchedElmStuff )

        RecieveReadMe (Ok content) ->
            ( { model | readMeContent = content }, Cmd.none )

        RecieveReadMe (Err _) ->
            ( { model | readMeContent = "Failed to fetch Readme content" }, Cmd.none )

        ToggleReadMe ->
            ( { model | showReadMe = not model.showReadMe }, Cmd.none )

        ToggleAndFetch ->
            let
                ( updatedModel, cmd1 ) = update ToggleReadMe model
                ( finalModel, cmd2) = update FetchReadMe updatedModel
            in
            ( finalModel, Cmd.batch [ cmd1, cmd2 ] )
        
        ToggleChoiceBox ->
            ( { model | showPopup = not model.showPopup }, Cmd.none )
```
*Fetching the markdown documents:*
```{elm}
fetchedReadMe : Cmd Msg
fetchedReadMe =
    Http.get
        { url = "https://raw.githubusercontent.com/Th0github/Elm-Kripke-Visualization/style/hoover-button/elm-front-end/src/Markdowns/HELP.md"
        , expect = Http.expectString RecieveReadMe
        }

fetchedElmStuff : Cmd Msg
fetchedElmStuff =
    Http.get
        { url = "https://raw.githubusercontent.com/Th0github/Elm-Kripke-Visualization/style/hoover-button/elm-front-end/src/Markdowns/ElmStuff.md"
        , expect = Http.expectString RecieveReadMe
        }
```
3. **View** function. Converts Markdown to HTML and render the view::
```{elm}
view : Model -> Html Msg
view model =
    div [ class "container-flex" ]
        [ div [ class "left-column" ]
            [ div [ class "head-container" ] 
                [ 
                text "Kripke Model Creator" 
                , button [ class "button", onMouseEnter ToggleChoiceBox, onClick ToggleAndFetch ] [ text "Toggle Help" ]
                , if model.showPopup then
                    div []
                        [ button [ class "button", onClick FetchReadMe ] [ text "Help Page" ]
                        , button [ class "button", onClick FetchElmStuffReadMe ] [ text "Elm Stuff" ]
                        ]
                  else
                    text "" 
                ]
        , div [ class "right-column" ]
            [ if model.showReadMe then
                div []
                    [ h1 [] [ text "Documentation" ]
                    , div [] [ lazy (Markdown.toHtml []) model.readMeContent ]
                    ]
```

```
5. **Main** function:
```{elm}
main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
```

6. Stylesheet (style.css):
```{css}
.button:hover {
  background-color: #367c39; /* Darker shade on hover */
}

.left-column {
  flex: 1;
  padding: 20px;
}

.right-column {
  flex: 1;
  padding: 20px;
  border-left: 1px solid #ccc;
  height: 100vh;
  overflow: auto;
}

.head-container { /*have button appear beside 'Kripke Model Creator' text. 
  display: flex;
  gap: 10px;
  padding: 10px;
  border-bottom: 1px solid #ccc;
}
```

This process showcases Elm's functional reactive programming approach, handling user interactions, making asynchronous HTTP requests, and dynamically updating the UI based on the application's state.
