#!/usr/bin/env bash

# Compiles the F-metric harness.
#
# Like tests/run-tests.sh, this copies the library from ../src into a local
# ELM_HOME so that the harness compiles against *this working copy* of
# elm-explorations/test rather than a published release. (The trick
# benchmarks/ uses -- putting ../src on source-directories -- can't work,
# because applications aren't allowed to import Elm.Kernel.* modules.)
#
# Compiles in dev mode, deliberately and with no option to change it: that is
# what node-test-runner does (lib/ElmCompiler.js only ever passes --output and
# --report), and it has to, because under --optimize Debug.toString returns
# "<internals>" for everything, so every counterexample elm-test reports would
# be unreadable. Dev mode is the production configuration here, not a
# compromise.
#
# With NOCLEANUP=1, keeps elm-stuff between builds. Faster, but only safe when
# ../src hasn't changed.
#
# The local ELM_HOME is reused across builds. Delete f-metric/elm_home to
# force a clean one (needs network access to repopulate).

set -euo pipefail

cd "${0%/*}"

PACKAGE_NAME=$(grep '"name"' ../elm.json | cut -d \" -f4)
PACKAGE_VERSION=$(grep '"version"' ../elm.json | cut -d \" -f4)
ELM_HOME_DIR="elm_home"
PACKAGE_PATH="${ELM_HOME_DIR}/0.19.2/packages/${PACKAGE_NAME}/${PACKAGE_VERSION}"

echo "Copying the library from ../src to ${PACKAGE_PATH}"
rm -rf "${PACKAGE_PATH}"
# Elm caches compiled dependency artifacts per package *version*, and the
# version never changes as we edit ../src, so without this the build silently
# reuses a stale copy of the library.
if [ -z "${NOCLEANUP+x}" ]; then
  rm -rf elm-stuff
fi
mkdir -p "${PACKAGE_PATH}"
cp -r ../src "${PACKAGE_PATH}/src"
cp ../elm.json ../README.md ../LICENSE "${PACKAGE_PATH}/"

echo "Compiling the harness"
ELM_HOME="${ELM_HOME_DIR}" elm make src/Main.elm --output elm.js

echo "Built f-metric/elm.js"
