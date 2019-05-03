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


type Effect x a
    -- = HttpRequestString {url : String, body : String} (Http.Response String -> Effect x a)
    -- | HttpRequestBytes {url : String, body : Bytes} (Http.Response Bytes -> Effect x a)
    -- | Now (Time.Posix -> Effect x a)
    -- | Succeed a
    -- | Fail x
    = Effect MagicalKernelType


fromCmd : Key -> Cmd msg -> Effect x msg c
fromCmd key cmd =
    Debug.todo "Turn a Cmd into an Effect using Kernel code"

fromTask : Key -> Task x a -> Effect x a c
fromTask key task =
    Debug.todo "Turn a Cmd into an Effect using Kernel code"


Expect.Effect.httpRequestString : ({url : String, body : String} -> (Http.Response String -> Effect x a) -> Expectation) -> Effect x a -> Expectation
Expect.Effect.httpRequestBytes : ({url : String, body : Bytes} (Http.Response Bytes -> Effect x a) -> Expectation) -> Effect x a -> Expectation
Expect.Effect.timeNow : ((Time.Posix -> Effect x a) -> Expectation) -> Effect x a -> Expectation
Expect.Effect.completed : (a -> Expectation) -> Effect x a -> Expectation
Expect.Effect.failed : (x -> Expectation) -> Effect x a -> Expectation

-- QUESTION: how does map2 work, given that order shouldn't matter?
-- QUESTION: can I say "these things happen in some order, but I don't care what order"
-- QUESTION: how can I say "I ran 2 HTTP requests, and the second one's url contains part of the response of the first one"
-- QUESTION: how should Cmd.batch and Task.map2 be different (if at all?)

let
    now = 12345789
in
update msg model      -- ( Model, Cmd Msg )
    |> Tuple.second   -- Cmd Msg
    |> fromCmd key    -- Effect x Msg
    |> Expect.Effect.timeNow
       (\nowCallback ->                              -- nowCallback : (Time.Posix -> Effect x Msg)
            Time.millisToPosix now
                |> nowCallback                       -- Effect x Msg
                |> Expect.Effect.httpRequestString
                   (\args httpCallback ->            -- httpCallback : (Http.Response String -> Effect x Msg)
                      let
                          currentTime = extractFromUrl args.url
                          response =
                              { body = Encode.encode 0 (Encode.object [ ("time", Encode.int currentTime) ]), status = 200, ... }
                      in
                      httpCallback response -- Effect x Msg
                          |> Expect.Effect.completed (Expect.equal (GotEcho (Ok now))))
                   )
       )


-- SCENARIO: run 3 HTTP requests in parallel, verify that at least 1 goes to fruits.com, at least one goes to noredink.com, at least one goes to elm-lan.org

-- Assertions:
-- 1. exactly 3 HTTP requests ran
-- 2. at least 1 went to fruits.com, it returned foo, and then it sent another HTTP request to plants.com with foo in the URL
-- 3. at least 1 went to noredink.com
-- 4. at least 1 went to elm-lang.org

update msg model                -- (Model, Cmd Msg)
    |> Tuple.second             -- Cmd Msg
    |> fromCmd key              -- Effect x Msg
    |> Expect.Effect.has3
        (Expect.Effect.httpRequest -> Expectation) -- succeeded once
        (Expect.Effect.httpRequest -> Expectation) -- succeeded once
        (Expect.Effect.httpRequest -> Expectation) -- succeeded once



-- SCENARIO: run 3 HTTP requests in parallel, verify that at least 1 goes to fruits.com, at least one goes to noredink.com, at least one goes to elm-lan.org

-- Assertions:
-- 1. exactly 3 HTTP requests ran
-- 2. at least 1 went to fruits.com
-- 3. at least 1 went to noredink.com
-- 4. at least 1 went to elm-lang.org

update msg model                -- (Model, Cmd Msg)
    |> Tuple.second             -- Cmd Msg
    |> fromCmd key              -- Effect x Msg
    |> Expect.Effect.all 
        [ Expect.Effect.httpRequestString (\{url} -> ( Expect.equal url "fruits.com", fruitsResp )) -- succeeded once
        , Expect.Effect.httpRequestString (\{url} -> ( Expect.equal url "nri.com", nriResp )) -- succeeded once
        , Expect.Effect.httpRequestString (\{url} -> ( Expect.equal url "elm-lang.org", elmResp )) -- succeeded once
        ]
    |> Expect.ok
    -- Effect x Msg

Task.map3
    (\elm fruits nri -> msg)
    elmlang
    fruitscom
    nri



                                                   -- succeeded twice == failure <--- prevent false positives
                                                   -- example: what if I give it (\_ -> Expect.pass) accidentally - might as well use has2 then

-- Run each callback on each task (so, 3 * 3 = 9 function invocations total)
-- Fail if any succeeds 0 times
-- Fail if any succeeds 2+ times
-- Fail if we get more total successes than the # of tasks that ran (so if you expect 4 tasks to run and it was a Task.map3, fail)

fromTask : Key -> Task x a -> Effect x a
fromTask key task =
    Debug.todo "Turn a Cmd into an Effect using Kernel code"

type ExpectedEffect

Expect.Effect.httpRequestString : ({url : String, body : String} -> ( Expectation, Http.Response String )) -> ExpectedEffect

Expect.Effect.httpRequestString : ({url : String, body : String} -> (Http.Response String -> Effect x a) -> Expectation) -> Effect x a -> Expectation
Expect.Effect.httpRequestBytes : ({url : String, body : Bytes} (Http.Response Bytes -> Effect x a) -> Expectation) -> Effect x a -> Expectation
Expect.Effect.all : List ExpectedEffect -> Effect x a -> Result x a
Expect.Effect.has3 : (b -> Expectation) -> (c -> Expectation) -> (d -> Expectation) -> Effect x a -> Effect x a
Expect.Effect.has2 : (a -> b ->  Expectation) -> (Effect x a -> Expectation) ->  (Effect x b -> Expectation) -> Expectation
Expect.Effect.timeNow : ((Time.Posix -> Effect x a) -> Expectation) -> Effect x a -> Expectation
Expect.Effect.completed : (a -> Expectation) -> Effect x a -> Expectation
Expect.Effect.failed : (x -> Expectation) -> Effect x a -> Expectation


-- SCENARIO: run a list of getElement tasks, folding over the list with Task.map2 to do them all in no particular order
-- https://package.elm-lang.org/packages/elm/browser/1.0.1/Browser-Dom#getElement


Expect.Effect.timeNow : Effect Never a Never -> Effect x Time.Posix ()
Expect.Effect.httpRequestString : Effect Never a Never -> Effect x (Http.Response String) {url : String, body : String}
Expect.Effect.followedBy : (Effect unbound1 unbound2 unbound3 -> Effect x2 a2 c2) -> (c1 -> a1) -> Effect x1 a1 c1 -> Effect x2 a2 c2

Expect.Effect.toResult : Effect x a c -> Result x a

-- With unexposed variant
update msg model                -- (Model, Cmd Msg)
    |> Tuple.second             -- Cmd Msg
    |> fromCmd key              -- Effect x Msg c
    |> Expect.Effect.toResult
    |> Expect.equal (Ok (GotTime 12345))


update msg model
    |> Tuple.second
    |> fromCmd key
    |> Expect.Effect.timeNow
    |> Expect.Effect.followedBy Expect.Effect.httpRequestString
        (\() -> Time.millisToPosix 123456789)
    |> Expect.Effect.toResult
    |> Result.map .url
    |> Expect.equal "/endpoint?time=123456789"


--- Writing our test

-- Asserting a single Cmd (not batched I guess) which makes an HTTP request to /graphql
-- Not asserting anything beyond that the Cmd was issued
case
    update msg model         -- (Model, Cmd Msg)
    |> Tuple.second      -- Cmd Msg
    |> fromCmd key       -- Effect never Msg
of
    HttpRequestString args _ ->
        Expect.equal args.url "/graphql"

    _ ->
        Expect.fail "Unexpected stuff?"

-- With unexposed variant
update msg model         -- (Model, Cmd Msg)
    |> Tuple.second      -- Cmd Msg
    |> fromCmd key       -- Effect never Msg
    |> Expect.Effect.httpRequestString (\args _ -> Expect.equal args.url "/graphql")


-- Same as above, but for (Task.perform GotTime Time.now)
case
    update msg model         -- (Model, Cmd Msg)
    |> Tuple.second      -- Cmd Msg
    |> fromCmd key       -- Effect never Msg
of
    Now _ ->
        Expect.pass

    _ ->
        Expect.fail "Unexpected stuff?"

-- With unexposed variant
update msg model         -- (Model, Cmd Msg)
    |> Tuple.second      -- Cmd Msg
    |> fromCmd key       -- Effect never Msg
    |> Expect.Effect.timeNow (\_ -> Expect.pass)


-- Getting time from Time.now and passing it to a callback that uses the
-- current time as a query param in a call to Http.task. Asserting that
-- the request URL includes the query param and then stopping
case
    update msg model
        |> Tuple.second
        |> fromCmd key
of
    Now callback ->
        case callback (Time.millisToPosix 123456789) of
            HttpRequestString args _ ->
                Expect.equal args.url "/endpoint?time=123456789"

            _ ->
                Expect.fail "Unexpected effect"

    _ ->
        Expect.fail "Unexpected effect"

-- With unexposed variant
update msg model
    |> Tuple.second
    |> fromCmd key
    |> Expect.Effect.timeNow
       (\callback ->
            Time.millisToPosix 123456789 -- Time.Posix
                |> callback -- Effect
                |> Expect.Effect.httpRequestString
                   (\{url} callback -> Expect.equal url "/endpoint?time=123456789")
       )

-- Getting time from Time.now and passing it to a callback that uses the
-- current time as a query param in a call to Http.task. Asserting that
-- the response is decoded to get the current time as echoed by the server.
case
    update msg model
        |> Tuple.second
        |> fromCmd key
of
    Now nowCallback ->
        case nowCallback (Time.millisToPosix 123456789) of
            HttpRequestString args httpCallback ->
                let
                    currentTime = extractFromUrl args.url
                    response =
                        { body = Encode.encode 0 (Encode.object [ ("time", Encode.int currentTime) ]), status = 200, ... }
                in
                Expect.equal
                    (httpCallback response)
                    (Succeed (GotEcho (Ok 123456789)))
            _ ->
                Expect.fail "Unexpected effect"

    _ ->
        Expect.fail "Unexpected effect"



let
    now = 12345789
in
update msg model
    |> Tuple.second
    |> fromCmd key
    |> Expect.Effect.timeNow
       (\nowCallback ->
            Time.millisToPosix now
                |> nowCallback
                |> Expect.Effect.httpRequestString
                   (\args httpCallback ->
                      let
                          currentTime = extractFromUrl args.url
                          response =
                              { body = Encode.encode 0 (Encode.object [ ("time", Encode.int currentTime) ]), status = 200, ... }
                      in
                      Expect.equal
                          (httpCallback response)
                          (Succeed (GotEcho (Ok now)))
                   )
       )

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
