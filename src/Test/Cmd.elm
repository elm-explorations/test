module Test.Cmd exposing
    ( ExpectCmd(..)
    , HttpBody(..)
    , HttpExpect(..)
    , HttpPart
    , Key
    , fromCmd
    , fuzz
    , fuzz2
    , test
    )

import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Expect exposing (Expectation)
import File exposing (File)
import Fuzz exposing (Fuzzer)
import Http
import Json.Decode exposing (Value)
import Random
import Task exposing (Task)
import Test exposing (Test)
import Test.Internal as Internal


type HttpExpect msg
    = ExpectString
        { body : String
        , simulateResponse : Http.Response String -> msg
        }
    | ExpectBytes
        { body : Bytes
        , simulateResponse : Http.Response Bytes -> msg
        }
    | ProcessSleep Float


expectSleep : a -> (Float -> a) -> ExpectCmd msg -> a
expectSleep : (Float -> Expectation) -> ExpectCmd msg -> Expectation
expectSleep : (Float -> Expectation) -> ExpectCmd msg -> Expectation
expectSleep : Key -> (Float -> Expectation) -> Cmd msg -> Expectation


expectHttpRequest :
    ({ method : String
     , headers : List ( String, String )
     , url : String
     , body : HttpBody
     , timeout : Maybe Float
     , tracker : Maybe String
     }
     -> Expectation
    )
    -> ExpectCmd msg
    -> Expectation


onHttpRequestString :
    ({ method : String
     , headers : List ( String, String )
     , url : String
     , body : HttpBody
     , timeout : Maybe Float
     , tracker : Maybe String
     }
     -> (Http.Response String -> msg)
     -> Expectation
    )
    -> ExpectCmd msg
    -> Expectation


onHttpRequestBytes :
    ({ method : String
     , headers : List ( String, String )
     , url : String
     , body : HttpBody
     , timeout : Maybe Float
     , tracker : Maybe String
     }
     -> (Http.Response Bytes -> msg)
     -> Expectation
    )
    -> ExpectCmd msg
    -> Expectation

-- |> Expect.Cmd.task key callback

expectTask : Key -> (a -> ...) -> Cmd msg -> Expectation


|> Expect.Task.fromCmd key
|> Expect.Task.now
    (\time maybeAndThenTask ->
        case maybeAndThenTask of
            Just nextTask ->
                Expect.Task.http
                    (\result maybeAndThenTask2 ->
                        ...
                    )
                    nextTask

            Nothing ->
                Expect.fail "I thought there was more"
    )

-- Simulating "Time.now"
|> Expect.Task.fromCmd key -- fromCmd : Key -> Cmd msg -> ExpectedTask msg
|> Expect.Task.now 1234567 -- now : Time.Posix -> ExpectedTask msg
|> Expect.Task.succeeds (GotTime 1234567) -- succeeds : msg -> ExpectedTask msg -> Expectation

-- Simulating "Time.now |> Task.andThen doSomeHttpRequest"
|> Expect.Task.fromCmd key
|> Expect.Task.now 1234567
|> Expect.Task.andThen
    (\maybeAndThenTask ->
        case maybeAndThenTask of
            Ok nextTask -> -- nextTask : ExpectedTask msg
                Expect.Task.httpRequest args
                    |> Expect.Task.succeeds (GotResponse blah)

            Err NotAndThen ->
                Expect.fail "I thought there was gonna be an andThen"
    )

-- Simulating "Time.now |> Task.andThen doSomeHttpRequest |> Task.andThen anotherHttpReq"
|> Expect.Task.fromCmd key
|> Expect.Task.now 1234567
|> Expect.Task.andThen
    (\maybeAndThenTask ->
        case maybeAndThenTask of
            Ok nextTask -> -- nextTask : ExpectedTask msg
                Expect.Task.httpRequest args
                    |> Expect.Task.andThen
                        (\maybeAndThenTask2 ->
                            Ok finalTask -> -- finalTask : ExpectedTask msg
                                finalTask
                                    |> Expect.Task.succeeds (GotResponse blah)

                            Err NotAndThen ->
                                Expect.fail "I thought there was gonna be an andThen"
                        )

            Err NotAndThen ->
                Expect.fail "I thought there was gonna be an andThen"
    )


Expect.Task.fromCmd : Key -> Cmd msg -> ExpectedTask msg

Expect.Task.now
    : Float 
    -> (Maybe (ExpectedTask msg) -> Expectation)
    -> ExpectedTask msg
    -> Expectation

-- Expect.Task.http
--     : (Result Http.Error Http.Response -> ExpectedTask msg)
--     -> ExpectedTask msg
--     -> Expectation

Expect.Task.succeed : ExpectedTask msg -> Maybe msg

Expect.Task.fail : ExpectedTask msg -> Maybe msg

Time.now
    |> Task.andThen (Http.get "blah.com" |> Http.toTask foo)


type ExpectCmd msg
    = None
    | Batch (List (ExpectCmd msg))
    | SendToPort { portName : String, payload : Value }
      -- elm/core
    | ProcessSpawn (Random.Generator Platform.ProcessId)
    | ProcessSleep Float
    | ProcessKill Platform.ProcessId
    | Task (ExpectTask Never msg)
      -- elm/browser
    | DomFocus String
    | DomBlur String
    | DomGetViewport
    | DomGetViewportOf String
    | DomSetViewport Float Float
    | DomSetViewportOf String Float Float
    | DomGetElement String
    | NavPushUrl String
    | NavReplaceUrl String
    | NavBack Int
    | NavForward Int
    | NavLoad String
    | NavReload
    | NavReloadAndSkipCache
      -- elm/bytes
    | GetHostEndianness
      -- elm/file
    | SelectFile { mimeTypes : List String, simulateLoaded : File -> msg }
    | SelectFiles { mimeTypes : List String, simulateLoaded : File -> List File -> msg }
    | DownloadString { filename : String, mimeType : String, content : String }
    | DownloadBytes { filename : String, mimeType : String, content : Bytes }
    | DownloadUrl String
      -- elm/http
    | HttpRequest
        -- TODO: can we distinguish between a Request and a RiskyRequest?
        { method : String
        , headers : List ( String, String )
        , url : String
        , body : HttpBody
        , timeout : Maybe Float
        , tracker : Maybe String
        }
        (HttpExpect msg)
    | CancelHttpRequest String
      -- elm/random
    | RandomGenerate (Random.Generator msg)
      -- elm/time
    | TimeHere
    | TimeNow
    | GetTimeZoneName


type ExpectTask x a
    = Success a
    | Failure x
    | AndThen a


type HttpBody
    = BytesBody Bytes
    | StringBody String
    | EmptyBody
    | Multipart (List HttpPart)


type alias HttpPart =
    { name : String
    , mimeType : Maybe String
    , filename : Maybe String
    , content : Bytes
    }


{-| Used in `fromCmd`. Obtain one by using `Test.Cmd.test`instead of `Test.test`, and same with the `fuzz` functions.
-}
type alias Key =
    Internal.Key


fromCmd : Key -> Cmd msg -> ExpectCmd msg
fromCmd key cmd =
    Debug.todo "implement"


fromTask : Key -> Task x a -> ExpectTask x a
fromTask key task =
    Debug.todo "implement"



------------------ EXAMPLE -----------------------


{-| Return a [`Test`](#Test) that evaluates a single
[`Expectation`](../Expect#Expectation).

    test "the empty list has 0 length" <|
        \key ->
            List.length []
                |> Expect.equal 0

-}
test : String -> (Key -> Expectation) -> Test
test =
    Debug.todo "implement"


{-| Take a function that produces a test, and calls it several (usually 100) times, using a randomly-generated input
from a [`Fuzzer`](http://package.elm-lang.org/packages/elm-community/elm-test/latest/Fuzz) each time. This allows you to
test that a property that should always be true is indeed true under a wide variety of conditions. The function also
takes a string describing the test.

These are called "[fuzz tests](https://en.wikipedia.org/wiki/Fuzz_testing)" because of the randomness.
You may find them elsewhere called [property-based tests](http://blog.jessitron.com/2013/04/property-based-testing-what-is-it.html),
[generative tests](http://www.pivotaltracker.com/community/tracker-blog/generative-testing), or
[QuickCheck-style tests](https://en.wikipedia.org/wiki/QuickCheck).

    import Test exposing (fuzz)
    import Fuzz exposing (list, int)
    import Expect


    fuzz (list int) "List.length should always be positive" <|
        -- This anonymous function will be run 100 times, each time with a
        -- randomly-generated fuzzList value.
        \fuzzList key ->
            fuzzList
                |> List.length
                |> Expect.atLeast 0

-}
fuzz :
    Fuzzer a
    -> String
    -> (a -> Key -> Expectation)
    -> Test
fuzz =
    Debug.todo "implement"


{-| Run a [fuzz test](#fuzz) using two random inputs.

This is a convenience function that lets you skip calling [`Fuzz.tuple`](Fuzz#tuple).

See [`fuzzWith`](#fuzzWith) for an example of writing this in tuple style.

    fuzz2 (list int) int "List.reverse never influences List.member" <|
        \nums target ->
            List.member target (List.reverse nums)
                |> Expect.equal (List.member target nums)

-}
fuzz2 :
    Fuzzer a
    -> Fuzzer b
    -> String
    -> (a -> b -> Key -> Expectation)
    -> Test
fuzz2 fuzzA fuzzB desc =
    Debug.todo "implement"
