# Elm REPL Worker

<br>


## What Is It?

An Elm `Platform.worker` that runs `elm repl` internally.
It communicates with your app via ports.

The REPL uses a [port of the Elm compiler](https://github.com/pithub/elm-compiler-in-elm)
from Haskell to Elm.

For more information see the recording of the August 2024 Elm Online Meetup:  
https://www.youtube.com/watch?v=OK9S_HUdReA.

<br>


## How to Use It?


#### Compile the Worker

```sh
make
```

or

```sh
elm make src/Repl/Worker.elm --output dist/worker.js
```

<br>


#### Add It to Your App

In branch `repl-worker-ui` there are two examples for adding
the REPL worker to an Elm app:

* [tea.html](https://github.com/pithub/elm-repl-worker/blob/repl-worker-ui/dist/tea.html) -
  Adds the REPL worker to a normal TEA app -
  [Live demo](https://pithub.github.io/elm-repl-worker/tea.html)

* [io.html](https://github.com/pithub/elm-repl-worker/blob/repl-worker-ui/dist/io.html) -
  Adds the REPL worker to an app using the IO monad of the Elm compiler port -
  [Live demo](https://pithub.github.io/elm-repl-worker/io.html)


<br>
