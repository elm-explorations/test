module Main (main) where

{-| HOW TO RUN THESE TESTS

$ npm test

Note that this always uses an initial seed of 902101337, since it can't do effects.

-}

import Platform as Platform
import Runner.Log as Runner.Log
import Runner.String (Summary)
import Runner.String as Runner.String
import SeedTests as SeedTests
import Tests as Tests


main :: Program {} {} msg
main =
    let
        program =
            Platform.worker
                { init : \{} -> ( {}, Cmd.none )
                , update : \_ {} -> ( {}, Cmd.none )
                , subscriptions : \{} -> Sub.none
                }
    in
    runAllTests program


runAllTests :: a -> a
runAllTests a =
    let
        runSeedTest =
            Runner.String.runWithOptions 1 SeedTests.fixedSeed

        _ =
            [ [ Runner.String.run Tests.all ]
            , List.map runSeedTest SeedTests.tests
            , List.map (runSeedTest >> removeAutoFail) SeedTests.noAutoFail
            ]
                |> List.concat
                |> List.foldl combineSummaries emptySummary
                |> Runner.Log.logOutput
    in
    a


emptySummary :: Summary
emptySummary =
    { output : "", passed : 0, failed : 0, autoFail : Nothing }


{-| Considers autoFail as pass so we can actually write tests about Test.skip
and Test.only which do not automatically fail.
-}
removeAutoFail :: Summary -> Summary
removeAutoFail summary =
    ( summary { autoFail = Nothing  })


combineSummaries :: Summary -> Summary -> Summary
combineSummaries first second =
    { output : first.output <> second.output
    , passed : first.passed + second.passed
    , failed : first.failed + second.failed
    , autoFail :
        case {a:first.autoFail, b:second.autoFail } of
            {a::Nothing, b::Nothing } ->
                Nothing

            ( Nothing, secondAutoFail ) ->
                secondAutoFail

            ( firstAutoFail, Nothing ) ->
                firstAutoFail

            {a::Just firstAutoFail, b::Just secondAutoFail } ->
                [ firstAutoFail, secondAutoFail ]
                    |> String.join "\n"
                    |> Just
    }
