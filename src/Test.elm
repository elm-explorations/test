module Test exposing
    ( Test, test
    , describe, concat, parameterized, todo, skip, only
    , fuzz, fuzz2, fuzz3, fuzzWith, FuzzOptions, fuzzWithExamples
    , Distribution, noDistribution, reportDistribution, expectDistribution
    )

{-| A module containing functions for creating and managing tests.

@docs Test, test


## Organizing Tests

@docs describe, concat, parameterized, todo, skip, only


## Fuzz Testing

@docs fuzz, fuzz2, fuzz3, fuzzWith, FuzzOptions, fuzzWithExamples
@docs Distribution, noDistribution, reportDistribution, expectDistribution

-}

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Set
import Test.Distribution exposing (ExpectedDistribution)
import Test.Distribution.Internal
import Test.Fuzz
import Test.Internal as Internal
import Test.Runner.Failure exposing (InvalidReason(..), Reason(..))


{-| A test which has yet to be evaluated. When evaluated, it produces one
or more [`Expectation`](../Expect#Expectation)s.

See [`test`](#test) and [`fuzz`](#fuzz) for some ways to create a `Test`.

-}
type alias Test =
    Internal.Test


{-| Run each of the given tests.

    concat [ testDecoder, testSorting ]

-}
concat : List Test -> Test
concat tests =
    if List.isEmpty tests then
        Internal.failNow
            { description = "This `concat` has no tests in it. Let's give it some!"
            , reason = Invalid EmptyList
            }

    else
        case Internal.duplicatedName tests of
            Err dups ->
                let
                    dupDescription duped =
                        "A test group contains multiple tests named '" ++ duped ++ "'. Do some renaming so that tests have unique names."
                in
                Internal.failNow
                    { description = String.join "\n" (List.map dupDescription <| Set.toList dups)
                    , reason = Invalid DuplicatedName
                    }

            Ok _ ->
                Internal.ElmTestVariant__Batch tests


{-| Apply a description to a list of tests.

    import Expect
    import Fuzz exposing (int)
    import Test exposing (describe, fuzz, test)


    describe "List"
        [ describe "reverse"
            [ test "has no effect on an empty list" <|
                \() ->
                    List.reverse []
                        |> Expect.equal []
            , fuzz "has no effect on a one-item list" int <|
                \num ->
                     List.reverse [ num ]
                        |> Expect.equal [ num ]
            ]
        ]

Passing an empty list will result in a failing test, because you either made a
mistake or are creating a placeholder.

-}
describe : String -> List Test -> Test
describe untrimmedDesc tests =
    let
        desc =
            String.trim untrimmedDesc
    in
    if String.isEmpty desc then
        Internal.failNow
            { description = "This `describe` has a blank description. Let's give it a useful one!"
            , reason = Invalid BadDescription
            }

    else if List.isEmpty tests then
        Internal.failNow
            { description = "This `describe " ++ desc ++ "` has no tests in it. Let's give it some!"
            , reason = Invalid EmptyList
            }

    else
        case Internal.duplicatedName tests of
            Err dups ->
                let
                    dupDescription duped =
                        "Contains multiple tests named '" ++ duped ++ "'. Let's rename them so we know which is which."
                in
                Internal.ElmTestVariant__Labeled desc <|
                    Internal.failNow
                        { description = String.join "\n" (List.map dupDescription <| Set.toList dups)
                        , reason = Invalid DuplicatedName
                        }

            Ok childrenNames ->
                if Set.member desc childrenNames then
                    Internal.ElmTestVariant__Labeled desc <|
                        Internal.failNow
                            { description = "The test '" ++ desc ++ "' contains a child test of the same name. Let's rename them so we know which is which."
                            , reason = Invalid DuplicatedName
                            }

                else
                    Internal.ElmTestVariant__Labeled desc (Internal.ElmTestVariant__Batch tests)


{-| Create a group of tests from a list of input-output cases (also called "parameterized tests").

    myTest : Test
    myTest =
        Test.parameterized "addition"
            [ ( 1, 1, 2 )
            , ( 5, 0, 5 )
            ]
        <|
            \( a, b, expectedSum ) ->
                Test.test (Debug.toString ( a, b )) <|
                    \() ->
                        (a + b)
                            |> Expect.equal expectedSum

Behaves like [`describe`](#describe): will fail if description is blank, if the
list is empty, or if test names are not unique.

-}
parameterized : String -> List a -> (a -> Test) -> Test
parameterized untrimmedDesc cases toTest =
    let
        desc =
            String.trim untrimmedDesc
    in
    if String.isEmpty desc then
        Internal.failNow
            { description = "This `parameterized` has a blank description. Let's give it a useful one!"
            , reason = Invalid BadDescription
            }

    else if List.isEmpty cases then
        Internal.failNow
            { description = "This `parameterized " ++ desc ++ "` has no test cases in it. Let's give it some!"
            , reason = Invalid EmptyList
            }

    else
        describe desc (List.map toTest cases)


{-| Return a [`Test`](#Test) that evaluates a single
[`Expectation`](../Expect#Expectation).

    import Expect
    import Test exposing (test)


    test "the empty list has 0 length" <|
        \() ->
            List.length []
                |> Expect.equal 0

-}
test : String -> (() -> Expectation) -> Test
test untrimmedDesc thunk =
    let
        desc =
            String.trim untrimmedDesc
    in
    if String.isEmpty desc then
        Internal.blankDescriptionFailure

    else
        Internal.ElmTestVariant__Labeled desc (Internal.ElmTestVariant__UnitTest (\() -> thunk ()))


{-| Returns a [`Test`](#Test) that is "TODO" (not yet implemented). These tests
always fail, but test runners will only include them in their output if there
are no other failures.

These tests aren't meant to be committed to version control. Instead, use them
when you're brainstorming lots of tests you'd like to write, but you can't
implement them all at once. When you replace `todo` with a real test, you'll be
able to see if it fails without clutter from tests still not implemented. But,
unlike leaving yourself comments, you'll be prompted to implement these tests
because your suite will fail.

    describe "a new thing"
        [ todo "does what is expected in the common case"
        , todo "correctly handles an edge case I just thought of"
        ]

This functionality is similar to "pending" tests in other frameworks, except
that a TODO test is considered failing but a pending test often is not.

-}
todo : String -> Test
todo desc =
    Internal.failNow
        { description = desc
        , reason = TODO
        }


{-| Returns a [`Test`](#Test) that causes other tests to be skipped, and
only runs the given one.

Calls to `only` aren't meant to be committed to version control. Instead, use
them when you want to focus on getting a particular subset of your tests to pass.
If you use `only`, your entire test suite will fail, even if
each of the individual tests pass. This is to help avoid accidentally
committing a `only` to version control.

If you you use `only` on multiple tests, only those tests will run. If you
put a `only` inside another `only`, only the outermost `only`
will affect which tests gets run.

See also [`skip`](#skip). Note that `skip` takes precedence over `only`;
if you use a `skip` inside an `only`, it will still get skipped, and if you use
an `only` inside a `skip`, it will also get skipped.

    describe "List"
        [ only <|
            describe "reverse"
                [ test "has no effect on an empty list" <|
                    \() ->
                        List.reverse []
                            |> Expect.equal []
                , fuzz "has no effect on a one-item list" int <|
                    \num ->
                        List.reverse [ num ]
                            |> Expect.equal [ num ]
                ]
        , test "This will not get run, because of the `only` above!" <|
            \() ->
                List.length []
                    |> Expect.equal 0
        ]

-}
only : Test -> Test
only =
    Internal.ElmTestVariant__Only


{-| Returns a [`Test`](#Test) that gets skipped.

Calls to `skip` aren't meant to be committed to version control. Instead, use
it when you want to focus on getting a particular subset of your tests to
pass. If you use `skip`, your entire test suite will fail, even if
each of the individual tests pass. This is to help avoid accidentally
committing a `skip` to version control.

See also [`only`](#only). Note that `skip` takes precedence over `only`;
if you use a `skip` inside an `only`, it will still get skipped, and if you use
an `only` inside a `skip`, it will also get skipped.

    describe "List"
        [ skip <|
            describe "reverse"
                [ test "has no effect on an empty list" <|
                    \() ->
                        List.reverse []
                            |> Expect.equal []
                , fuzz "has no effect on a one-item list" int <|
                    \num ->
                        List.reverse [ num ]
                            |> Expect.equal [ num ]
                ]
        , test "This is the only test that will get run; the other was skipped!" <|
            \() ->
                List.length []
                    |> Expect.equal 0
        ]

-}
skip : Test -> Test
skip =
    Internal.ElmTestVariant__Skipped


{-| Options [`fuzzWith`](#fuzzWith) accepts.


### `runs`

The number of times to run each fuzz test. (Default is 100.)

    import Expect
    import Fuzz exposing (int, list)
    import Test exposing (fuzzWith, noDistribution)

    fuzzWith "List.length should never be negative"
        { runs = 350, distribution = noDistribution }
        (list int) <|
        -- This anonymous function will be run 350 times, each time with a
        -- randomly-generated fuzzList value. (It will always be a list of ints
        -- because of (list int) above.)
        \fuzzList ->
            fuzzList
                |> List.length
                |> Expect.atLeast 0


### `distribution`

A way to report/enforce a statistical distribution of your input values.
(Default is `noDistribution`.)

    import Expect
    import Fuzz exposing (int, list)
    import Test exposing (expectDistribution, fuzzWith)
    import Test.Distribution

    fuzzWith "Sum > Average"
        { runs = 350
        , distribution =
            expectDistribution
                [ ( Test.Distribution.zero, "empty", \xs -> List.length xs == 0 )
                , ( Test.Distribution.atLeast 10, "3+ items", \xs -> List.length xs >= 3 )
                ]
        }
        (list int)
    <|
        \xs ->
            List.sum xs
                |> Expect.greaterThan (average xs)

-}
type alias FuzzOptions a =
    { runs : Int
    , distribution : Distribution a
    }


{-| Run a [`fuzz`](#fuzz) test with the given [`FuzzOptions`](#FuzzOptions).

Note that there is no `fuzzWith2`, but you can always pass more fuzz values in
using [`Fuzz.pair`](Fuzz#pair), [`Fuzz.triple`](Fuzz#triple),
for example like this:

    import Expect
    import Fuzz exposing (int, list, pair)
    import Test exposing (fuzzWith, noDistribution)


    fuzzWith "List.reverse never influences List.member"
        { runs = 4200, distribution = noDistribution }
        (pair (list int) int) <|
            \(nums, target) ->
                List.member target (List.reverse nums)
                    |> Expect.equal (List.member target nums)

-}
fuzzWith : String -> FuzzOptions a -> Fuzzer a -> (a -> Expectation) -> Test
fuzzWith desc options fuzzer getTest =
    if options.runs < 1 then
        Internal.failNow
            { description = "Fuzz tests must have a run count of at least 1, not " ++ String.fromInt options.runs ++ "."
            , reason = Invalid NonpositiveFuzzCount
            }

    else
        fuzzWithHelp options (Test.Fuzz.fuzzTest desc options.distribution fuzzer getTest)


fuzzWithHelp : FuzzOptions a -> Test -> Test
fuzzWithHelp options aTest =
    case aTest of
        Internal.ElmTestVariant__UnitTest _ ->
            aTest

        Internal.ElmTestVariant__FuzzTest run ->
            Internal.ElmTestVariant__FuzzTest (\seed _ -> run seed options.runs)

        Internal.ElmTestVariant__Labeled label subTest ->
            Internal.ElmTestVariant__Labeled label (fuzzWithHelp options subTest)

        Internal.ElmTestVariant__Skipped subTest ->
            -- It's important to treat skipped tests exactly the same as normal,
            -- until after seed distribution has completed.
            fuzzWithHelp options subTest
                |> Internal.ElmTestVariant__Only

        Internal.ElmTestVariant__Only subTest ->
            fuzzWithHelp options subTest
                |> Internal.ElmTestVariant__Only

        Internal.ElmTestVariant__Batch tests ->
            tests
                |> List.map (fuzzWithHelp options)
                |> Internal.ElmTestVariant__Batch


{-| Specify hardcoded example inputs for your fuzz test. They will be run
alongside the randomized inputs.

This is handy when a fuzz test has found a regression: you can add the found
value as an example.

    Test.fuzzWithExamples "compare with zero is only EQ when input also is zero"
        { runs = 100, distribution = Test.noDistribution }
        Fuzz.float
        [ ( "NaN", 0 / 0 )
        , ( "Infinity", 1 / 0 )
        ]
    <|
        \input ->
            let
                expect =
                    if input == 0 && input == input then
                        Expect.equal

                    else
                        Expect.notEqual
            in
            compare input 0
                |> Expect.expect EQ

-}
fuzzWithExamples : String -> FuzzOptions a -> Fuzzer a -> List ( String, a ) -> (a -> Expectation) -> Test
fuzzWithExamples desc options fuzzer examples getTest =
    let
        labels =
            List.map Tuple.first examples |> Set.fromList

        -- Just in case the examples are `[ ( "fuzz", a ), ( "fuzz_", b ) ]`.
        -- Sibling tests can’t have the same label.
        fuzzLabel label =
            if Set.member label labels then
                fuzzLabel (label ++ "_")

            else
                label
    in
    describe desc
        (List.map (\( name, value ) -> test name (\() -> getTest value)) examples
            ++ [ fuzzWith (fuzzLabel "fuzz") options fuzzer getTest ]
        )


{-| Take a function that produces a test, and calls it several (usually 100) times, using a randomly-generated input
from a [`Fuzzer`](http://package.elm-lang.org/packages/elm-explorations/test/latest/Fuzz) each time. This allows you to
test that a property that should always be true is indeed true under a wide variety of conditions. The function also
takes a string describing the test.

These are called "[fuzz tests](https://en.wikipedia.org/wiki/Fuzz_testing)" because of the randomness.
You may find them elsewhere called [property-based tests](http://blog.jessitron.com/2013/04/property-based-testing-what-is-it.html),
[generative tests](http://www.pivotaltracker.com/community/tracker-blog/generative-testing), or
[QuickCheck-style tests](https://en.wikipedia.org/wiki/QuickCheck).

    import Expect
    import Fuzz exposing (int, list)
    import Test exposing (fuzz)

    fuzz "List.length should never be negative" (list int) <|
        -- This anonymous function will be run 100 times, each time with a
        -- randomly-generated fuzzList value.
        \fuzzList ->
            fuzzList
                |> List.length
                |> Expect.atLeast 0

-}
fuzz :
    String
    -> Fuzzer a
    -> (a -> Expectation)
    -> Test
fuzz desc fuzzer getExpectation =
    Test.Fuzz.fuzzTest desc Test.Distribution.Internal.NoDistributionNeeded fuzzer getExpectation


{-| Run a [fuzz test](#fuzz) using two random inputs.

This is a convenience function that lets you skip calling [`Fuzz.pair`](Fuzz#pair).

See [`fuzzWith`](#fuzzWith) for an example of writing this using tuples.

    import Expect
    import Fuzz exposing (int, list)
    import Test exposing (fuzz2)


    fuzz2 "List.reverse never influences List.member" (list int) int <|
        \nums target ->
            List.member target (List.reverse nums)
                |> Expect.equal (List.member target nums)

-}
fuzz2 :
    String
    -> Fuzzer a
    -> Fuzzer b
    -> (a -> b -> Expectation)
    -> Test
fuzz2 desc fuzzA fuzzB getExpectation =
    fuzz desc (Fuzz.pair fuzzA fuzzB) (\( a, b ) -> getExpectation a b)


{-| Run a [fuzz test](#fuzz) using three random inputs.

This is a convenience function that lets you skip calling [`Fuzz.triple`](Fuzz#triple).

-}
fuzz3 :
    String
    -> Fuzzer a
    -> Fuzzer b
    -> Fuzzer c
    -> (a -> b -> c -> Expectation)
    -> Test
fuzz3 desc fuzzA fuzzB fuzzC getExpectation =
    fuzz desc (Fuzz.triple fuzzA fuzzB fuzzC) (\( a, b, c ) -> getExpectation a b c)



-- Distribution --


{-| With `Distribution` you can observe statistics about your fuzz test inputs and
assert that a given proportion of test cases belong to a given class.

  - `noDistribution` opts out of these checks.

  - `reportDistribution` will collect statistics and report them after the test
    runs (both when it passes and fails) and so is mostly useful as a temporary
    setting when creating your fuzzers and tests.

  - `expectDistribution` will collect statistics, but only report them (and fail
    the test) if the `ExpectedDistribution` is not met. Handy for checking your
    fuzzers are giving interesting and relevant inputs to your tests.

```elm
fuzzWith "description" { runs = 10000, distribution = noDistribution }

fuzzWith "description"
    { runs = 10000
    , distribution =
        reportDistribution
            [ ( "fizz", \n -> (n |> modBy 3) == 0 )
            , ( "buzz", \n -> (n |> modBy 5) == 0 )
            , ( "even", \n -> (n |> modBy 2) == 0 )
            , ( "odd", \n -> (n |> modBy 2) == 1 )
            ]
    }

fuzzWith "description"
    { runs = 10000
    , distribution =
        expectDistribution
            [ ( Test.Distribution.atLeast 30, "fizz", \n -> (n |> modBy 3) == 0 )
            , ( Test.Distribution.atLeast 15, "buzz", \n -> (n |> modBy 5) == 0 )
            , ( Test.Distribution.moreThanZero, "fizz buzz", \n -> (n |> modBy 15) == 0 )
            , ( Test.Distribution.zero, "outside range", \n -> n < 1 || n > 20 )
            ]
    }
```

The `a` type variable in `Distribution a` is the same type as your fuzzed type.

For example, if you're fuzzing a String with `Fuzzer String` and want to see
distribution information for values produced by this fuzzer, you need to provide
`String -> Bool` functions to your `reportDistribution` or `expectDistribution` calls,
which will in turn produce a `Distribution String`.

-}
type alias Distribution a =
    Test.Distribution.Internal.Distribution a


{-| Opts out of the test input distribution checking.
-}
noDistribution : Distribution a
noDistribution =
    Test.Distribution.Internal.NoDistributionNeeded


{-| Collects statistics and reports them after the test runs (both when it passes
and fails).
-}
reportDistribution : List ( String, a -> Bool ) -> Distribution a
reportDistribution =
    Test.Distribution.Internal.ReportDistribution


{-| Collects statistics and makes sure the expected distribution is met.

Fails the test and reports the distribution if the expected distribution is not met.

Uses a statistical test to make sure the distribution doesn't pass or fail the
distribution by accident (a flaky test). Will run more tests than specified with the
`runs` config option if needed.

This has the consequence of running more tests the closer your expected distribution
is to the true distribution. You can thus minimize and speed up this
"making sure" process by requesting slightly less % of your distribution than
needed.

Currently the statistical test is tuned to allow a false positive/negative in
1 in every 10^9 tests.

-}
expectDistribution : List ( ExpectedDistribution, String, a -> Bool ) -> Distribution a
expectDistribution =
    Test.Distribution.Internal.ExpectDistribution
