/*

import Elm.Kernel.Utils exposing (Tuple0, Tuple2)
import Result exposing (Err, Ok)
import Dict exposing (toList)
import Set exposing (toList)

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


// DEEP EQUALITY WITH NUMBER TOLERANCE
//
// This is a copy of `_Utils_eq` / `_Utils_eqHelp` from elm/core, with one
// difference: whenever it reaches two numbers, it compares them with the
// given tolerances and NaN behavior instead of with `===`. This lets us
// compare numbers inside arbitrary data structures (records, custom types,
// lists, dicts, ...) the way `Expect.within` compares two bare Floats.
//
// Note Elm's `Int` and `Float` are both JS numbers, so the tolerance
// unavoidably applies to `Int`s too.


var _Test_deepEqual = F5(function(nansAreEqual, absoluteTolerance, relativeTolerance, x, y)
{
  var config = {
    nansAreEqual: nansAreEqual,
    absolute: absoluteTolerance,
    relative: relativeTolerance
  };

  var pair, stack = [];

  if (!_Test_deepEqualHelp(x, y, config, 0, stack))
  {
    return false;
  }

  while (pair = stack.pop())
  {
    if (!_Test_deepEqualHelp(pair.a, pair.b, config, 0, stack))
    {
      return false;
    }
  }

  return true;
});


function _Test_deepEqualHelp(x, y, config, depth, stack)
{
  if (typeof x === 'number' && typeof y === 'number')
  {
    return _Test_numbersEqual(x, y, config);
  }

  if (x === y)
  {
    return true;
  }

  if (typeof x !== 'object' || typeof y !== 'object' || x === null || y === null)
  {
    return false;
  }

  if (depth > 100)
  {
    stack.push(__Utils_Tuple2(x, y));
    return true;
  }

  /**__DEBUG/
  if (x.$ === 'Set_elm_builtin')
  {
    x = __Set_toList(x);
    y = __Set_toList(y);
  }
  if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
  {
    x = __Dict_toList(x);
    y = __Dict_toList(y);
  }
  //*/

  /**__PROD/
  if (x.$ < 0)
  {
    x = __Dict_toList(x);
    y = __Dict_toList(y);
  }
  //*/

  // Any difference in the constructor tags will be caught here too, since `$`
  // is one of the keys we're iterating over.
  for (var key in x)
  {
    if (!_Test_deepEqualHelp(x[key], y[key], config, depth + 1, stack))
    {
      return false;
    }
  }

  return true;
}


function _Test_numbersEqual(x, y, config)
{
  // NaN is the only value that isn't equal to itself.
  if (x !== x || y !== y)
  {
    return config.nansAreEqual && x !== x && y !== y;
  }

  // Exact equality (this is also what makes Infinity == Infinity).
  if (x === y)
  {
    return true;
  }

  var difference = Math.abs(x - y);

  return difference <= config.absolute
    || difference <= Math.abs(x * config.relative)
    || difference <= Math.abs(y * config.relative);
}


// RUNTIME INTROSPECTION
//
// `Expect.equal` has the type `a -> a -> Expectation`, so by the time a
// comparison fails we have no type information to tell us whether we're
// looking at a List, a Dict, a Set, ... and thus which kind of diff to show.
// These two functions let Test.Internal.Equality figure that out at runtime.


function _Test_tag(value)
{
  return (typeof value === 'object' && value !== null && value.$ !== undefined)
    ? String(value.$)
    : '';
}


function _Test_unsafeCoerce(value)
{
  return value;
}


// Whether the constructor tags we get from `_Test_tag` are the descriptive
// names we can recognize (`'::'`, `'Set_elm_builtin'`, ...) or the small
// per-type integers that `--optimize` replaces them with. With the integers,
// a `List` is indistinguishable from any other two-field constructor, so the
// structure detection has to give up. See Test.Internal.Equality.
function _Test_canDetectStructure()
{
  /**__DEBUG/
  return true;
  //*/

  /**__PROD/
  return false;
  //*/
}
