#!/usr/bin/env bash

set -ex

rm -f elm.js
rm -Rf elm-stuff
rm -Rf elm_home/0.19.1/packages/elm-explorations/test

mkdir -p elm_home
mkdir -p elm_home/0.19.1/packages

PACKAGE_VERSION=`grep \"version ../elm.json | cut -d \" -f 4`
mkdir -p elm_home/0.19.1/packages/elm-explorations/test/${PACKAGE_VERSION}
rsync -va ../ elm_home/0.19.1/packages/elm-explorations/test/${PACKAGE_VERSION}/ --exclude tests --exclude elm-stuff --exclude .git --exclude node_modules

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
cp ./versions.dat elm_home/0.19.1/packages/versions.dat

export ELM_HOME="$(pwd)"/elm_home

# randomize the seeds before running
grep -ERiIl 'initialSeed [0-9]+' src |
while read -r FILE
do
  SEED=$(head -200 /dev/urandom | cksum | cut -f1 -d " ")
  sed -i.bak "s/initialSeed [0-9]\+/initialSeed ${SEED}/g" "${FILE}"
done

elm make src/Main.elm --output elm.js

# restore seeds back
find . -type f -name '*.bak' |
while read -r FILE
do
  mv "${FILE}" "${FILE//.bak/}"
done

# node -prof elm.js # for performance testing, combine with node --prof-process tests/isolate-* > test-processed.txt
node elm.js
