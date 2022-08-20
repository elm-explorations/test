#!/usr/bin/env bash

# With RANDOMIZED=1, replaces all Random.initialSeed calls with a random integer.
# With SEED=3937524181, replaces all Random.initialSeed calls with that seed.
# With NOCLEANUP=1, won't remove elm-stuff etc. (tests should be a bit faster as a result)

DIM="\e[2m";
COLOR_OFF="\e[0m";

echo -en "${DIM}";
echo "Navigating to the tests/ directory"
cd "${0%/*}" # Change current working directory to this one.

PACKAGE_NAME=$(grep '"name"' ../elm.json | cut -d \" -f4)
PACKAGE_VERSION=$(grep '"version"' ../elm.json | cut -d \" -f4)
ELM_HOME="elm_home"
PACKAGE_PATH="${ELM_HOME}/0.19.1/packages/${PACKAGE_NAME}/${PACKAGE_VERSION}"

if [ -z ${NOCLEANUP+x} ]; then
  echo "Removing temporary files from previous runs"
  rm -rf elm.js
  rm -rf elm-stuff
  rm -rf "${PACKAGE_PATH}"
fi

echo "Copying the library from .. to ${PACKAGE_PATH}"
mkdir -p "${PACKAGE_PATH}"
cp -r ../src       "${PACKAGE_PATH}/src"
cp -r ../elm.json  "${PACKAGE_PATH}/elm.json"
cp -r ../README.md "${PACKAGE_PATH}/README.md"
cp -r ../LICENSE   "${PACKAGE_PATH}/LICENSE"

if [[ "${RANDOMIZED}" -eq 1 ]]; then
  SEED=$(head -200 /dev/urandom | cksum | cut -f1 -d " ")
  echo "Randomizing seed to ${SEED}"
fi

if [ ! -z ${SEED+x} ]; then
  echo "Setting all Random.initialSeed calls to ${SEED}"
  grep -ERiIl 'initialSeed [0-9]+' src | grep -v SeedTest | while read -r FILE; do
    sed -i.bak "s/initialSeed [0-9]\+/initialSeed ${SEED}/g" "${FILE}"
  done
fi

echo "Compiling the test suite with ELM_HOME=${ELM_HOME}"
echo -en "${COLOR_OFF}";
ELM_HOME="${ELM_HOME}" elm make src/Main.elm --output elm.js
echo -en "${DIM}";

if [ ! -z ${SEED+x} ]; then
  echo "Restoring back original Random.initialSeed calls"
  find . -type f -name '*.bak' | while read -r FILE; do
    mv "${FILE}" "${FILE//.bak/}"
  done
fi

if [ ! -f elm.js ]; then
    echo "Compilation failed"
    echo -en "${COLOR_OFF}";
    exit 1
fi

echo "Running the compiled test suite"
echo "----------------------------------------------------"
echo -en "${COLOR_OFF}";
node elm.js 2>&1
