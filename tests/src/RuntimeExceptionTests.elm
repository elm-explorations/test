module RuntimeExceptionTests exposing (all)

import Expect
import Random
import Test exposing (..)
import Test.Runner exposing (SeededRunners(..))
import Test.Runner.Failure


toSeededRunners : Test -> SeededRunners
toSeededRunners =
    Test.Runner.fromTest 5 (Random.initialSeed 42)


all : Test
all =
    describe "catching exceptions"
        [ test "when a test raises an exception, it is turned into a failure" <|
            \() ->
                case toSeededRunners (test "crashes" <| \() -> Debug.todo "crash") of
                    Plain [ runner ] ->
                        runner.run ()
                            |> List.head
                            |> Maybe.andThen Test.Runner.getFailureReason
                            |> Expect.equal
                                (Just
                                    { given = Nothing

                                    -- The line number is currently off-by-one because we use Elm 0.19.2 and it has a bug:
                                    -- https://github.com/elm/compiler/issues/2358
                                    , description = "This test failed because it threw an exception: \"Error: TODO in module `RuntimeExceptionTests` on line 19\n\ncrash\""
                                    , reason = Test.Runner.Failure.Custom
                                    }
                                )

                    Plain runners ->
                        Expect.fail ("Expected SeededRunner to have one runner, but had " ++ Debug.toString runners)

                    val ->
                        Expect.fail ("Expected SeededRunner to be Plain, but was " ++ Debug.toString val)
        ]
