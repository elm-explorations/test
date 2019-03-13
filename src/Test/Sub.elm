module Test.Sub exposing (ExpectSub(..), Key, fromSub)

import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Http
import Test exposing (Test)
import Test.Internal as Internal


type ExpectSub msg
    = HttpTrack
        { tracker : String
        , simulateProgress : Http.Progress -> msg
        }
    | Batch (List (ExpectSub msg))


{-| Used in `fromCmd`. Obtain one by using `Test.Cmd.test`instead of `Test.test`, and same with the `fuzz` functions.
-}
type alias Key =
    Internal.Key


fromSub : Key -> Sub msg -> ExpectSub msg
fromSub key sub =
    Debug.todo "implement"
