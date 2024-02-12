module Test.Internal (Test(..), blankDescriptionFailure, duplicatedName, failNow, toString) where

import Random as Random
import Set (Set)
import Set as Set
import Test.Expectation (Expectation)
import Test.Expectation as Test.Expectation
import Test.Runner.Failure (InvalidReason(..), Reason(..))
import Test.Runner.Failure as Test.Runner.Failure


{-| All variants of this type has the `ElmTestVariant__` prefix so that
node-test-runner can recognize them in the compiled JavaScript. This lets us
add more variants here without having to update the runner.

For more information, see <https://github.com/elm-explorations/test/pull/153>

-}
data Test
    = ElmTestVariant__UnitTest ({} -> List Expectation)
    | ElmTestVariant__FuzzTest (Random.Seed -> Int -> List Expectation)
    | ElmTestVariant__Labeled String Test
    | ElmTestVariant__Skipped Test
    | ElmTestVariant__Only Test
    | ElmTestVariant__Batch (List Test)


{-| Create a test that always fails for the given reason and description.
-}
failNow :: { description :: String, reason :: Reason } -> Test
failNow record =
    ElmTestVariant__UnitTest
        (\{} -> [ Test.Expectation.fail record ])


blankDescriptionFailure :: Test
blankDescriptionFailure =
    failNow
        { description : "This test has a blank description. Let's give it a useful one!"
        , reason : Invalid BadDescription
        }


duplicatedName :: List Test -> Result (Set String) (Set String)
duplicatedName tests =
    let
        names :: Test -> List String
        names test =
            case test of
                ElmTestVariant__Labeled str _ ->
                    [ str ]

                ElmTestVariant__Batch subtests ->
                    List.concatMap names subtests

                ElmTestVariant__UnitTest _ ->
                    List.nil

                ElmTestVariant__FuzzTest _ ->
                    List.nil

                ElmTestVariant__Skipped subTest ->
                    names subTest

                ElmTestVariant__Only subTest ->
                    names subTest

        accumDuplicates :: String -> {a::Set String, b::Set String } -> {a::Set String, b::Set String }
        accumDuplicates newName {a:dups, b:uniques } =
            if Set.member newName uniques then
                {a:Set.insert newName dups, b:uniques }

            else
                {a:dups, b:Set.insert newName uniques }

        {a:dupsAccum, b:uniquesAccum } =
            List.concatMap names tests
                |> List.foldl accumDuplicates {a:Set.empty, b:Set.empty }
    in
    if Set.isEmpty dupsAccum then
        Ok uniquesAccum

    else
        Err dupsAccum


toString :: a -> String
toString =
    Elm.Kernel.Debug.toString
