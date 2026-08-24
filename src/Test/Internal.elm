module Test.Internal exposing (Test, TestVariant(..), blankDescriptionFailure, duplicatedName, failNow, identifyTest, toString, unwrapTestVariant, wrapTestVariant, wrapWithTryCatch)

import Elm.Kernel.Test
import Random
import RandomRun exposing (RandomRun)
import Set exposing (Set)
import Task exposing (Task)
import Test.Expectation exposing (Expectation, FuzzTestExpectation)
import Test.Runner.Failure exposing (InvalidReason(..), Reason(..))


{-| Opaque type around tests. Use `wrapTestVariant` to create a `Test`
from a `TestVariant`. It tags the test (using Kernel code) so that
`identifyTest` can recognize it later. This allows test runners to
find exposed values of type `Test` without having to implement type
inference.
-}
type Test
    = Test TestVariant


{-| All variants of this type have the `ElmTestVariant__` prefix for
backwards compatibility with test runners. Test runners use the prefix
to recognize tests in the compiled JavaScript.

For more information, see <https://github.com/elm-explorations/test/pull/153>

The new way of recognizing tests is using the `identifyTest` function, as
explained in the `Test` type. The prefix can be removed in a major version
if we want to clean up the code internally.

-}
type TestVariant
    = ElmTestVariant__UnitTest (() -> Expectation)
    | ElmTestVariant__FuzzTest (Maybe Int) (Random.Seed -> Int -> List Int -> FuzzTestExpectation)
    | ElmTestVariant__Labeled String Test
    | ElmTestVariant__Tagged String Test
    | ElmTestVariant__Skipped Test
    | ElmTestVariant__Only Test
    | ElmTestVariant__Batch (List Test)


wrapTestVariant : TestVariant -> Test
wrapTestVariant testVariant =
    Elm.Kernel.Test.tagTest (Test testVariant)


unwrapTestVariant : Test -> TestVariant
unwrapTestVariant (Test testVariant) =
    testVariant


identifyTest : a -> Maybe Test
identifyTest =
    Elm.Kernel.Test.identifyTest


{-| Create a test that always fails for the given reason and description.
-}
failNow : { description : String, reason : Reason } -> Test
failNow record =
    ElmTestVariant__UnitTest
        (\() -> Test.Expectation.fail record)
        |> wrapTestVariant


blankDescriptionFailure : Test
blankDescriptionFailure =
    failNow
        { description = "This test has a blank description. Let's give it a useful one!"
        , reason = Invalid BadDescription
        }


duplicatedName : List Test -> Result (Set String) (Set String)
duplicatedName tests =
    let
        names : Test -> List String
        names (Test test) =
            case test of
                ElmTestVariant__Labeled str _ ->
                    [ str ]

                ElmTestVariant__Tagged _ _ ->
                    []

                ElmTestVariant__Batch subtests ->
                    List.concatMap names subtests

                ElmTestVariant__UnitTest _ ->
                    []

                ElmTestVariant__FuzzTest _ _ ->
                    []

                ElmTestVariant__Skipped subTest ->
                    names subTest

                ElmTestVariant__Only subTest ->
                    names subTest

        accumDuplicates : String -> ( Set String, Set String ) -> ( Set String, Set String )
        accumDuplicates newName ( dups, uniques ) =
            if Set.member newName uniques then
                ( Set.insert newName dups, uniques )

            else
                ( dups, Set.insert newName uniques )

        ( dupsAccum, uniquesAccum ) =
            List.concatMap names tests
                |> List.foldl accumDuplicates ( Set.empty, Set.empty )
    in
    if Set.isEmpty dupsAccum then
        Ok uniquesAccum

    else
        Err dupsAccum


toString : a -> String
toString =
    Elm.Kernel.Debug.toString


runWithTryCatch : (a -> b) -> a -> Result String b
runWithTryCatch =
    Elm.Kernel.Test.runWithTryCatch


wrapWithTryCatch : (a -> Expectation) -> (a -> Expectation)
wrapWithTryCatch getExpectation =
    \a ->
        case runWithTryCatch getExpectation a of
            Ok expectation ->
                expectation

            Err message ->
                Test.Expectation.fail
                    { description = "This test failed because it threw an exception: \"" ++ message ++ "\""
                    , reason = Custom
                    }
