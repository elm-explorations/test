module File exposing (AbsolutePath, FileError(..), RelativePath, readFile, writeFile, writeTempFile)

import Elm.Kernel.Test


type FileError
    = FileNotFound
    | IsDirectory
    | PathEscapesDirectory
    | GeneralFileError String


type alias RelativePath =
    String


type alias AbsolutePath =
    String


{-| Read the contents of the filePath relative to "tests/"
-}
readFile : RelativePath -> Result FileError ( AbsolutePath, String )
readFile =
    Elm.Kernel.Test.readFile


{-| Write the contents of the second argument to the file path in the first argument relative to "tests/"

Returns the absolute file path if successful.

-}
writeFile : RelativePath -> String -> Result FileError AbsolutePath
writeFile =
    Elm.Kernel.Test.writeFile


{-| Write the contents of the second argument to the file path in the first argument relative to a temp directory

Returns the absolute file path if successful.

-}
writeTempFile : RelativePath -> String -> Result FileError AbsolutePath
writeTempFile =
    Elm.Kernel.Test.writeTempFile
