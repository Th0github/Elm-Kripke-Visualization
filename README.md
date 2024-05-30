# Elm Kripke Visualization

## How to run the application

### Elm

First install the Elm compiler (see https://guide.elm-lang.org/install/)
To compile the new version for Elm front-end:

```bash
elm make src/Main.elm --main.js
```

With runnning

```bash
elm reactor
```

You can open the index.html and see the web page

### Haskell

The Haskell back-end is called web-app

After installing stack (see https://www.haskell.org/), you can build the application with

```bash
stack build
```

After that you run the application with

```bash
stack run web-app-exe
```
