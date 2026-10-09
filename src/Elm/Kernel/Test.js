/*

import Maybe exposing (Just, Nothing)
import Result exposing (Err, Ok)

*/

var _Test_runWithTryCatch = F2(function(thunk, a)
{
  try {
    // Attempt to run the thunk as normal.
    return __Result_Ok(thunk(a));
  } catch (err) {
    // If it throws, return an error instead of crashing.
    return __Result_Err(err.toString());
  }
});

var _Test_symbol = Symbol("_Test_symbol");

function _Test_tagTest(test)
{
  test[_Test_symbol] = true;
  return test;
}

function _Test_identifyTest(value)
{
  return value && value[_Test_symbol] ? __Maybe_Just(value) : __Maybe_Nothing;
}
