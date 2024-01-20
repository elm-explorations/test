module Test.Internal exposing (Test, TestData(..), blankDescriptionFailure, duplicatedName, failNow, toString, wrapTest, unwrapTest)

import Random
import Set exposing (Set)
import Test.Expectation exposing (Expectation)
import Test.Runner.Failure exposing (InvalidReason(..), Reason(..))
import Elm.Kernel.Test


type TestData
    = UnitTest (() -> List Expectation)
    | FuzzTest (Random.Seed -> Int -> List Expectation)
    | Labeled String TestData
    | Skipped TestData
    | Only TestData
    | Batch (List TestData)


{-| Newtype wrapper around TestData so that
node-test-runner can recognize them in the compiled JavaScript. 

**MUST** only be constructed by the kernel
-}
type Test
    = Wrapped TestData


{-| Create a test that always fails for the given reason and description.
-}
failNow : { description : String, reason : Reason } -> TestData
failNow record =
    UnitTest
        (\() -> [ Test.Expectation.fail record ])


blankDescriptionFailure : TestData
blankDescriptionFailure =
    failNow
        { description = "This test has a blank description. Let's give it a useful one!"
        , reason = Invalid BadDescription
        }


duplicatedName : List Test -> Result (Set String) (Set String)
duplicatedName tests =
    let
        names : TestData -> List String
        names test =
            case test of
                Labeled str _ ->
                    [ str ]

                Batch subtests ->
                    List.concatMap names subtests

                UnitTest _ ->
                    []

                FuzzTest _ ->
                    []

                Skipped subTest ->
                    names subTest

                Only subTest ->
                    names subTest

        accumDuplicates : String -> ( Set String, Set String ) -> ( Set String, Set String )
        accumDuplicates newName ( dups, uniques ) =
            if Set.member newName uniques then
                ( Set.insert newName dups, uniques )

            else
                ( dups, Set.insert newName uniques )

        ( dupsAccum, uniquesAccum ) =
            tests
                |> List.map (\(Wrapped td) -> td)
                |> List.concatMap names
                |> List.foldl accumDuplicates ( Set.empty, Set.empty )
    in
    if Set.isEmpty dupsAccum then
        Ok uniquesAccum

    else
        Err dupsAccum


toString : a -> String
toString =
    Elm.Kernel.Debug.toString


wrapTest : TestData -> Test
wrapTest td =
    Elm.Kernel.Test.tagTest (Wrapped td)


unwrapTest : Test -> TestData
unwrapTest (Wrapped t) =
    t

