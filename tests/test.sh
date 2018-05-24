#!/bin/sh

set -ex

rm -f elm.js
rm -Rf elm-stuff
rm -Rf elm_home/0.19.0/package/elm-explorations/test

mkdir -p elm_home
mkdir -p elm_home/0.19.0/package
mkdir -p elm_home/0.19.0/package/elm-explorations/test/1.0.0

rsync -va ../ elm_home/0.19.0/package/elm-explorations/test/1.0.0/ --exclude tests --exclude elm-stuff --exclude .git

export ELM_HOME="$(pwd)"/elm_home

elm make --debug src/Main.elm --output elm.js

node elm.js
