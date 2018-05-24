module Helpers exposing (different, expectPass, randomSeedFuzzer, same, succeeded, testStringLengthIsPreserved)

-- import Test.Expectation exposing (Expectation(..))

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Random
import Shrink
import Test exposing (Test)
import Test.Runner
import Test.Runner.Failure exposing (Reason(..))


expectPass : a -> Expectation
expectPass _ =
    Expect.pass


testStringLengthIsPreserved : List String -> Expectation
testStringLengthIsPreserved strings =
    strings
        |> List.map String.length
        |> List.sum
        |> Expect.equal (String.length (List.foldl (++) "" strings))



-- expectToFail : Test -> Test
-- expectToFail expectation =
--     case Test.Runner.getFailureReason expectation of
--         Nothing ->
--             Expect.fail "Expected this test to fail, but it passed!"
--         Just _ ->
--             Expect.pass


succeeded : Expectation -> Bool
succeeded expectation =
    case Test.Runner.getFailureReason expectation of
        Nothing ->
            True

        Just _ ->
            False


passesToFails :
    ({ reason : Reason
     , description : String
     , given : Maybe String
     }
     -> Maybe String
    )
    -> List Expectation
    -> List Expectation
passesToFails f expectations =
    expectations
        |> List.filterMap (passToFail f)
        |> List.map Expect.fail
        |> (\list ->
                if List.isEmpty list then
                    [ Expect.pass ]

                else
                    list
           )


passToFail :
    ({ reason : Reason
     , description : String
     , given : Maybe String
     }
     -> Maybe String
    )
    -> Expectation
    -> Maybe String
passToFail f expectation =
    case Test.Runner.getFailureReason expectation of
        Nothing ->
            Just "Expected this test to fail, but it passed!"

        Just record ->
            f record



-- expectFailureHelper : ({ description : String, given : Maybe String, reason : Reason } -> Maybe String) -> Test -> Test
-- expectFailureHelper f test =
--     case Test.Runner.getFailureReason test of
--         Internal.UnitTest runTest ->
--             Internal.UnitTest <|
--                 \() ->
--                     passesToFails f (runTest ())
--         Internal.FuzzTest runTest ->
--             Internal.FuzzTest <|
--                 \seed runs ->
--                     passesToFails f (runTest seed runs)
--         Internal.Labeled desc labeledTest ->
--             Internal.Labeled desc (expectFailureHelper f labeledTest)
--         Internal.Batch tests ->
--             Internal.Batch (List.map (expectFailureHelper f) tests)
--         Internal.Skipped subTest ->
--             expectFailureHelper f subTest
--                 |> Internal.Skipped
--         Internal.Only subTest ->
--             expectFailureHelper f subTest
--                 |> Internal.Only
-- testShrinking : Test -> Test
-- testShrinking =
--     let
--         handleFailure { given, description } =
--             let
--                 acceptable =
--                     String.split "|" description
--             in
--             case given of
--                 Nothing ->
--                     Just "Expected this test to have a given value!"
--                 Just g ->
--                     if List.member g acceptable then
--                         Nothing
--                     else
--                         Just <| "Got shrunken value " ++ g ++ " but expected " ++ String.join " or " acceptable
--     in
--     expectFailureHelper handleFailure


{-| get a good distribution of random seeds, and don't shrink our seeds!
-}
randomSeedFuzzer : Fuzzer Random.Seed
randomSeedFuzzer =
    Fuzz.custom (Random.int 0 0xFFFFFFFF) Shrink.noShrink |> Fuzz.map Random.initialSeed


same : Expectation -> Expectation -> Expectation
same a b =
    case ( Test.Runner.getFailureReason a, Test.Runner.getFailureReason b ) of
        ( Nothing, Nothing ) ->
            Expect.pass

        ( Just _, Just _ ) ->
            Expect.pass

        ( reasonA, reasonB ) ->
            Expect.equal reasonA reasonB
                |> Expect.onFail "expected both arguments to fail, or both to succeed"


different : Expectation -> Expectation -> Expectation
different a b =
    case ( Test.Runner.getFailureReason a, Test.Runner.getFailureReason b ) of
        ( Nothing, Just _ ) ->
            Expect.pass

        ( Just _, Nothing ) ->
            Expect.pass

        ( Nothing, Nothing ) ->
            Expect.fail "expected only one argument to fail, but both passed"

        ( Just reasonA, Just reasonB ) ->
            [ reasonA, reasonB ]
                |> Expect.equal []
                |> Expect.onFail "expected only one argument to fail"
