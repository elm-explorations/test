/*

import Elm.Kernel.Utils exposing (Tuple0, Tuple2)
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


const fs = require('node:fs');
const path = require('node:path');
const os = require('node:os');
const process = require('node:process');
const crypto = require('node:crypto');

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
        return __Result_Ok(__Utils_Tuple2(fullPath, fs.readFileSync(fullPath, { encoding: 'utf8' })));
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

function WriteFile(root, filePath, contents)
{
    // Test for this early as `resolve` will strip training slashes
    if (filePath.slice(-1) == path.sep) {
        return __Result_Err(__File_IsDirectory);
    }

    // Protect against writing files above the root directory
    const fullPath = path.resolve(root, filePath);

    if (!fullPath.startsWith(root))
    {
        return __Result_Err(__File_PathEscapesDirectory);
    }

    const fullDir = path.dirname(fullPath);

    // Note that this does not throw an error if the directory exists
    fs.mkdirSync(fullDir, {recursive: true});

    try {
        fs.writeFileSync(fullPath, contents);
        return __Result_Ok(fullPath);
    }
    catch (err)
    {   
        return __Result_Err(__File_GeneralFileError(err.toString()));
    }
}

var _Test_writeFile = F2(function(filePath, contents)
{
    return WriteFile(path.resolve("tests"), filePath, contents);
})

var tempDir = null;
var _Test_writeTempFile = F2(function(filePath, contents)
{
    if (tempDir === null)
    {
        tempDir = os.tmpdir() + "/" + crypto.randomUUID();
        fs.mkdirSync(tempDir);
    }

    return WriteFile(tempDir, filePath, contents);
})

var overwriteGoldenFiles = null;
function _Test_overwriteGoldenFiles(unused)
{
    if (overwriteGoldenFiles === null)
        overwriteGoldenFiles = process.env.OVERWRITE_GOLDEN_FILES == '1';
    
    return overwriteGoldenFiles;
}