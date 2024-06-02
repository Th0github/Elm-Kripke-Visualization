# Elm Kripke Visualization

## How to run the application

### Elm

First install the Elm compiler (see https://guide.elm-lang.org/install/)
If you are in the main directory of this project, go to the following subdirectory:

```bash
cd elm-front-end
```

In this directory, you can compile the new version of the front-end project through:

```bash
elm make src/Main.elm --output main.js
```

After compiling the main.js, you can start the webpage through the following commands.
First, start the development server of Elm through the following command.

```bash
elm reactor
```

It start a development server on your machine on port 8000.
If you open the following address through your prefered web browser `localhost:8000`, you can open the `index.html` file and see the web page

### Haskell

The Haskell back-end is called web-app
First you install stack (see https://www.haskell.org/).

When you are in the main directory of the application, you go to `web-app` directory.
In this directory, you can build the application with

```bash
stack build
```

After that you run the application within the same directory:

```bash
stack run web-app-exe
```
