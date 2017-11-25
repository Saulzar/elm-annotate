#!/bin/bash

cabal new-build
/home/oliver/sync/elm-annotate/dist-newstyle/build/elm-annotate-0.1.0.0/build/generate-types/generate-types > client/Types.elm
elm-make client/Main.elm --output html/elm.js
