# Elm-Kripke-Visualization

## Data model

```json
{
    "worlds": [Integer],
    "relations": [
        {
            "agentName": string,
            "worldRelations": [[Integer]]
        }
    ],
    "valuations": {
        "world": Integer,
        "propostions": [Integer]
    }
}

```

# How to run the program

## Elm
The Elm application can be opened in browser by openning the `index.html`.

To compile the Elm application run `elm make src/Main.elm --output elm.js`.

To compile the test, you will need elm-test and the test can be run with `npx elm-test`
