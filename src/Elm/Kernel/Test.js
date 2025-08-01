/*

import Elm.Kernel.Utils exposing (Tuple0)
import File exposing (FileNotFound, GeneralFileError, IsDirectory, PathEscapesDirectory)
import Result exposing (Err, Ok)

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


const fs = require('fs');
const path = require('path');

function _Test_readFile(filePath)
{
    // Test for this early as `resolve` will strip training slashes
    if (filePath.slice(-1) == path.sep) {
        return __Result_Err(__File_IsDirectory);
    }

    // Protect against reading files above the "tests" directory
    const testsPath = path.resolve("tests");
    const fullPath = path.resolve(testsPath, filePath);

    if (!fullPath.startsWith(testsPath))
    {
        return __Result_Err(__File_PathEscapesDirectory);
    }

    try {
        return __Result_Ok(fs.readFileSync(fullPath, { encoding: 'utf8' }));
    }
    catch (err)
    {
        if (err.code == "ENOENT"){
            return __Result_Err(__File_FileNotFound);
        }
        else {
            return __Result_Err(__File_GeneralFileError(err.toString()));
        }
    }
}

var _Test_writeFile = F2(function(filePath, contents)
{
    // Test for this early as `resolve` will strip training slashes
    if (filePath.slice(-1) == path.sep) {
        return __Result_Err(__File_IsDirectory);
    }

    // Protect against writing files above the "tests" directory
    const testsPath = path.resolve("tests");
    const fullPath = path.resolve(testsPath, filePath);

    if (!fullPath.startsWith(testsPath))
    {
        return __Result_Err(__File_PathEscapesDirectory);
    }

    const fullDir = path.dirname(fullPath);

    if (!fs.existsSync(fullDir))
    {
        // Can this make a nested directory? 
        fs.mkdirSync(fullDir, {recursive: true});
    }

    try {
        fs.writeFileSync(fullPath, contents);
        return __Result_Ok(__Utils_Tuple0);
    }
    catch (err)
    {   
        return __Result_Err(__File_GeneralFileError(err.toString()));
    }
})