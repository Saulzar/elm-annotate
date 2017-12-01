#!/bin/bash

cabal new-build && `find ./dist-newstyle -type f -name generate-types` > client/Types.elm
# elm-make client/Main.elm --output html/elm.js
