module Test.Task exposing
    ( ExpectTask(..)
    , Key
    , fromTask
    , fuzz
    , fuzz2
    , test
    )

import Expect exposing (Expectation)
import Task exposing (Task)
import Test exposing (Test)
import Test.Internal as Internal


type
    ExpectTask x a
    -- TODO Q: does this need to be opaque for andThen to work?
    = Success a
      -- TODO Q: should we let you inspect individual tasks?
    | Failure x


andThen : (a -> ExpectTask x b) -> ExpectTask x a -> ExpectTask x b
andThen fn task =
    Debug.todo "implement"


{-| Used in `fromTask`. Obtain one by using `Test.Task.test`instead of `Test.test`, and same with the `fuzz` functions.
-}
type alias Key =
    Internal.Key


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
