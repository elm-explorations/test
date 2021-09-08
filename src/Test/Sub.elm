module Test.Sub exposing (ExpectSub(..), Key, fromSub)

import Browser.Events
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Http
import Json.Decode exposing (Decoder, Value)
import Test exposing (Test)
import Test.Internal as Internal
import Time


type ExpectSub msg
    = None
    | Batch (List (ExpectSub msg))
    | SubscribeToPort { portName : String, simulateGet : Value -> msg }
      -- elm/http
    | HttpTrack
        { tracker : String
        , simulateProgress : Http.Progress -> msg
        }
      -- elm/browser
    | OnAnimationFrame (Time.Posix -> msg)
    | OnAnimationFrameDelta (Float -> msg)
    | OnKeyPress (Decoder msg)
    | OnKeyDown (Decoder msg)
    | OnKeyUp (Decoder msg)
    | OnClick (Decoder msg)
    | OnMouseMove (Decoder msg)
    | OnMouseDown (Decoder msg)
    | OnMouseUp (Decoder msg)
    | OnResize (Int -> Int -> msg)
    | OnVisibilityChange (Browser.Events.Visibility -> msg)
      -- elm/time
    | Every Float (Time.Posix -> msg)


{-| Used in `fromCmd`. Obtain one by using `Test.Cmd.test`instead of `Test.test`, and same with the `fuzz` functions.
-}
type alias Key =
    Internal.Key


fromSub : Key -> Sub msg -> ExpectSub msg
fromSub key sub =
    Debug.todo "implement"
