#!/usr/bin/env bash

set -eu

# Check that this script is run from top level directory
if [[ $(git rev-parse --show-toplevel) != $(pwd) ]]; then
  >&2 echo "Please run ./tests/make.sh from git root directory!"
  exit 1
fi

FILE_WITH_IMPORT=src/Test/Internal.elm

sed -i.bak 's/import Elm\.Kernel\.Debug/-- import Elm\.Kernel\.Debug/g' "$FILE_WITH_IMPORT"

elm make --docs=documentation.json
exit_code=$?

mv "$FILE_WITH_IMPORT.bak" "$FILE_WITH_IMPORT"

exit $exit_code
