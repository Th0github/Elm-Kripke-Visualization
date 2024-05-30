# Elm Stuff
To have reached this page, you have hovered over the toggle button, pressed a button request to a Markdown document, which Elm then converted to HTML to be displayed.

A break down how this happened in Elm syntax is given below:
1. Hover Over Toggle Button:
We include the following types in our Msg
```{elm}
type Msg = RecieveReadMe (Result Http.Error String)
    | ToggleReadMe
    | FetchReadMe
    | ShowFetchButton
    | HideFetchButton
    | Show
```

```{elm}
view = 
```

2. Press Button Request:
Upon pressing the button, an Elm event handler captures this action.
```{elm}
update = 
view =
```
3. Request to Markdown Document:
```{elm}
fetchedReadMe : Cmd Msg
fetchedReadMe =
    Http.get
        { url = "https://raw.githubusercontent.com/elm/browser/master/README.md"
        , expect = Http.expectString RecieveReadMe
        }

update msg model =
  case msg of 
    FetchReadMe ->
      ( model, fetchedReadMe )

    RecieveReadMe (Ok content) ->
      ( { model | readMeContent = content }, Cmd.none )

   RecieveReadMe (Err _) ->
      ( { model | readMeContent = "Failed to fetch Readme content" }, Cmd.none )
```
4. Converted Markdown to Html
```{elm}
update msg model =
  case msg of
    ToggleReadMe ->
      ( { model | showReadMe = not model.showReadMe }, Cmd.none )


```
6. Display the HTML in the right side:
```{elm}
```

This process showcases Elm's functional reactive programming approach, handling user interactions, making asynchronous HTTP requests, and dynamically updating the UI based on the application's state.
