#!/bin/bash

set -eu

sed -i 's/import Elm\.Kernel\.Debug/-- import Elm\.Kernel\.Debug/g' src/Test/Internal.elm

elm make --docs=documentation.json
exit_code=$?

sed -i 's/-- import Elm\.Kernel\.Debug/import Elm\.Kernel\.Debug/g' src/Test/Internal.elm

exit $exit_code
