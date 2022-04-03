#!/usr/bin/env bash

DIM="\e[2m";
COLOR_OFF="\e[0m";

echo -en "${DIM}";
echo "Navigating to the tests/ directory"
cd "${0%/*}" # Change current working directory to this one.

PACKAGE_NAME=$(grep '"name"' ../elm.json | cut -d \" -f4)
PACKAGE_VERSION=$(grep '"version"' ../elm.json | cut -d \" -f4)
ELM_HOME="elm_home"
PACKAGE_PATH="${ELM_HOME}/0.19.1/packages/${PACKAGE_NAME}/${PACKAGE_VERSION}"

echo "Removing temporary files from previous runs"
rm -rf elm.js
rm -rf elm-stuff
rm -rf "${PACKAGE_PATH}"

echo "Copying the library from .. to ${PACKAGE_PATH}"
mkdir -p "${PACKAGE_PATH}"
cp -r ../src       "${PACKAGE_PATH}/src"
cp -r ../elm.json  "${PACKAGE_PATH}/elm.json"
cp -r ../README.md "${PACKAGE_PATH}/README.md"
cp -r ../LICENSE   "${PACKAGE_PATH}/LICENSE"

echo "Compiling the test suite with ELM_HOME=${ELM_HOME}"
echo -en "${COLOR_OFF}";
ELM_HOME="${ELM_HOME}" elm make src/Main.elm --output elm.js

if [ ! -f elm.js ]; then
    echo -en "${DIM}";
    echo "Compilation failed"
    echo -en "${COLOR_OFF}";
    exit 1
fi

echo -en "${DIM}";
echo "Running the test suite"
echo "----------------------------------------------------"
echo -en "${COLOR_OFF}";
node elm.js
