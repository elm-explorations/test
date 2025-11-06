module File exposing (AbsolutePath, FileError(..), RelativePath, overwriteGoldenFiles, readFile, writeFile, deleteFile)

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

{-| Delete the file specified in filePath relative to "tests/" -}
deleteFile : RelativePath -> Result FileError ()
deleteFile = 
    Elm.Kernel.Test.deleteFile

{-| Checks the OVERWRITE\_GOLDEN\_FILES environment variable
-}
overwriteGoldenFiles : () -> Bool
overwriteGoldenFiles =
    Elm.Kernel.Test.overwriteGoldenFiles
