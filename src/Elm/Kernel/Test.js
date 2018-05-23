/*

import Elm.Kernel.Utils exposing (Tuple0)
import Elm.Kernel.List exposing (Nil, Cons)
import Expect exposing (fail)

*/


// RUN

function _Test_runThunk(thunk)
{
  try {
    // Attempt to run the thunk as normal.
    return thunk(__Utils_Tuple0);
  } catch (err) {
    // If it throws, return a test failure instead of crashing.
    var message = 'This test failed because it threw an exception: "' + err + '"';
    return __List_Cons(__Expect_fail(message), __List_Nil);
  }
}
