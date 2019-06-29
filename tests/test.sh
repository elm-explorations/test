#!/bin/sh

set -ex

rm -f elm.js
rm -Rf elm-stuff
rm -Rf elm_home/0.19.0/package/elm-explorations/test

mkdir -p elm_home
mkdir -p elm_home/0.19.0/package

PACKAGE_VERSION="1.2.2"
mkdir -p elm_home/0.19.0/package/elm-explorations/test/${PACKAGE_VERSION}
rsync -va ../ elm_home/0.19.0/package/elm-explorations/test/${PACKAGE_VERSION}/ --exclude tests --exclude elm-stuff --exclude .git --exclude node_modules

if which runhaskell; then
    # this produces ./versions.dat, but that file
    # is checked in, so it's fine to skip regenerating if
    # haskell is not available (such as on CI)
    runhaskell MakeTestRegistry.hs
elif which stack; then
    stack runhaskell MakeTestRegistry.hs
else
    echo "$0: WARNING: Neither runhaskell or stack are available on your PATH, so I can't regenerate versions.dat"
fi
cp ./versions.dat elm_home/0.19.0/package/versions.dat

export ELM_HOME="$(pwd)"/elm_home

elm make src/Main.elm --output elm.js

node elm.js
