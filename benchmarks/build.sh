#!/usr/bin/env bash

# Compiles the throughput benchmarks.
#
# Copies the library from ../src into a local ELM_HOME so we benchmark *this*
# working copy rather than a published release. The old approach here -- putting
# ../src on source-directories -- cannot work: applications aren't allowed to
# import Elm.Kernel.* modules, so it stopped compiling as soon as RandomRun
# moved into the kernel.
#
# Elm caches compiled dependency artifacts per package *version*, and the
# version never changes as we edit ../src, so elm-stuff has to go too or the
# build silently reuses a stale library.

set -euo pipefail

cd "${0%/*}"

PACKAGE_NAME=$(grep '"name"' ../elm.json | cut -d \" -f4)
PACKAGE_VERSION=$(grep '"version"' ../elm.json | cut -d \" -f4)
ELM_HOME_DIR="elm_home"
PACKAGE_PATH="${ELM_HOME_DIR}/0.19.2/packages/${PACKAGE_NAME}/${PACKAGE_VERSION}"

rm -rf "${PACKAGE_PATH}" elm-stuff
mkdir -p "${PACKAGE_PATH}"
cp -r ../src "${PACKAGE_PATH}/src"
cp ../elm.json ../README.md ../LICENSE "${PACKAGE_PATH}/"

ELM_HOME="${ELM_HOME_DIR}" elm make Main.elm --output elm.js

echo "Built benchmarks/elm.js"
