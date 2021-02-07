module Test.Internal exposing (Key(..), Test(..), blankDescriptionFailure, duplicatedName, failNow, toString)

import Elm.Kernel.Debug
import Random exposing (Generator)
import Set exposing (Set)
import Test.Expectation exposing (Expectation(..))
import Test.Runner.Failure exposing (InvalidReason(..), Reason(..))


{-| All variants of this type has the `ElmTestVariant__` prefix so that
node-test-runner can recognize them in the compiled JavaScript. This lets us
add more variants here without having to update the runner.

For more information, see <https://github.com/elm-explorations/test/pull/153>

-}
type Test
    = ElmTestVariant__UnitTest (() -> List Expectation)
    | ElmTestVariant__FuzzTest (Random.Seed -> Int -> List Expectation)
    | ElmTestVariant__Labeled String Test
    | ElmTestVariant__Skipped Test
    | ElmTestVariant__Only Test
    | ElmTestVariant__Batch (List Test)


{-| Proof that we're running a test right now.
Used with fromCmd and fromSub, to grant them permission to inspect Cmd and Sub values.
-}
type Key
    = Key


{-| Create a test that always fails for the given reason and description.
-}
failNow : { description : String, reason : Reason } -> Test
failNow record =
    ElmTestVariant__UnitTest
        (\() -> [ Test.Expectation.fail record ])


blankDescriptionFailure : Test
blankDescriptionFailure =
    failNow
        { description = "This test has a blank description. Let's give it a useful one!"
        , reason = Invalid BadDescription
        }


duplicatedName : List Test -> Result String (Set String)
duplicatedName =
    let
        names : Test -> List String
        names test =
            case test of
                ElmTestVariant__Labeled str _ ->
                    [ str ]

                ElmTestVariant__Batch subtests ->
                    List.concatMap names subtests

                ElmTestVariant__UnitTest _ ->
                    []

                ElmTestVariant__FuzzTest _ ->
                    []

                ElmTestVariant__Skipped subTest ->
                    names subTest

                ElmTestVariant__Only subTest ->
                    names subTest

        insertOrFail : String -> Result String (Set String) -> Result String (Set String)
        insertOrFail newName =
            Result.andThen
                (\oldNames ->
                    if Set.member newName oldNames then
                        Err newName

                    else
                        Ok <| Set.insert newName oldNames
                )
    in
    List.concatMap names
        >> List.foldl insertOrFail (Ok Set.empty)


toString : a -> String
toString =
    Elm.Kernel.Debug.toString
