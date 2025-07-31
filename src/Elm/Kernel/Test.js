/*

import Elm.Kernel.Utils exposing (Tuple0)
import Result exposing (Err, Ok)

*/

const fs = require('fs');

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

function _Test_readFile(filePath)
{
    try {
        return __Result_Ok(fs.readFileSync(filePath, { encoding: 'utf8' }));
    }
    catch (err)
    {
        return __Result_Err(err.toString())
    }
}

var _Test_writeFile = F2(function(filePath, contents)
{
    try {
        fs.writeFileSync(filePath, contents);
        return __Result_Ok(__Utils_Tuple0);
    }
    catch (err)
    {
        return __Result_Err(err.toString());
    }
})