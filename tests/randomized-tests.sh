#!/usr/bin/env bash

# Run a randomized test suite until a test failure happens

cd "${0%/*}" # Change current working directory to this one.

echo "Running test suite with randomized seeds until we find one where some tests fail. Each run will add one dot here:" 

DATE=$(date +%F-%T)

mkdir -p failing-runs/

while true;
do 
  SEED=$(head -200 /dev/urandom | cksum | cut -f1 -d " ")
  FILE="failing-runs/$DATE-$SEED.txt"
  TIME=$( ( time NOCLEANUP=1 SEED="$SEED" ./run-tests.sh 2>&1 1>"$FILE"; exit ${PIPESTATUS[0]} ) 2>&1 )
  #[ $? == 0 ] && (echo -n '.') || (echo -n '!'; echo $TIME >>$FILE)
  [ $? == 0 ] && (echo -n '.'; rm $FILE) || (echo -n '!'; echo $TIME >>$FILE)
  sleep 0.1
done
