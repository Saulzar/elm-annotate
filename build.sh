#!/bin/bash

cabal new-build 
cabal new-build --ghcjs

cp `find ./dist-newstyle -type f -name all.js` ./html
