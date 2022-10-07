/*

import Elm.Kernel.Utils exposing (Tuple0)
import Result exposing (Err, Ok)
import Maybe exposing (Just, Nothing)

*/


function _Test_runThunk(thunk)
{
  try {
    // Attempt to run the thunk as normal.
    return __Result_Ok(thunk(__Utils_Tuple0));
  } catch (err) {
    // If it throws, return an error instead of crashing.
    return __Result_Err(err.toString());
  }
}

var _Test_elmTestSymbol = Symbol("elmTestSymbol");

function _Test_tagTest(test)
{
  test[__elmTestSymbol] = true;
  return test;
}

function _Test_downcastTest(value)
{
  return value && value[__elmTestSymbol] ? $elm$core$Maybe$Just(value) : $elm$core$Maybe$Nothing;
}

