module File exposing (readFile, writeFile, FileError(..))

import Elm.Kernel.Test

type FileError
    = FileNotFound
    | IsDirectory
    | PathEscapesDirectory
    | GeneralFileError String

readFile : String -> Result FileError String
readFile = Elm.Kernel.Test.readFile

writeFile : String -> String -> Result FileError ()
writeFile = Elm.Kernel.Test.writeFile