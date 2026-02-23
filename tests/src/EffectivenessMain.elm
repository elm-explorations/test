module EffectivenessMain exposing (main)

{-| Effectiveness test runner: finds the minimal fuzz count per seed
to reach a target number of failures, then reports min/max/avg/median.

Configure targetFailures, startSeed, endSeed, and maxFuzz at the top.

Build: elm make src/EffectivenessMain.elm --output effectiveness-runner.js
Run: node effectiveness-runner.js

(From the tests/ directory, with ELM\_HOME set like run-tests.sh if needed.)

-}

import EffectivenessTests
import Platform
import Random
import Runner.String exposing (Summary)



-- Config (hardcoded; change and recompile to adjust)


targetFailures : Int
targetFailures =
    10


startSeed : Int
startSeed =
    1


endSeed : Int
endSeed =
    100


maxFuzz : Int
maxFuzz =
    1000000


main : Program () () ()
main =
    Platform.worker
        { init = \() -> ( (), Cmd.none )
        , update = \_ s -> ( s, Cmd.none )
        , subscriptions = \() -> Sub.none
        }
        |> runEffectiveness


runEffectiveness : a -> a
runEffectiveness a =
    let
        seedRange =
            List.range startSeed endSeed

        totalSeeds =
            List.length seedRange
    in
    let
        _ =
            Debug.log
                ("Target {target} failures, seeds {start}..{end}"
                    |> String.replace "{target}" (String.fromInt targetFailures)
                    |> String.replace "{start}" (String.fromInt startSeed)
                    |> String.replace "{end}" (String.fromInt endSeed)
                )
                ()
    in
    let
        minimalFuzzValues =
            progressRunSeeds totalSeeds seedRange
    in
    let
        _ =
            report seedRange minimalFuzzValues
    in
    a


{-| Run each seed with progress logs (seed k/T: starting → result).
-}
progressRunSeeds : Int -> List Int -> List Int
progressRunSeeds totalSeeds seeds =
    let
        digits = String.length (String.fromInt totalSeeds)

        step seed ( k, acc ) =
            let
                result =
                    minimalFuzzForSeed targetFailures maxFuzz seed
                _ =
                    case result of
                        Just n ->
                            let 
                                _ = 
                                    Debug.log
                                        ("  Seed {seed}: minimal fuzz"
                                            |> String.replace "{seed}" (String.padLeft digits ' ' (String.fromInt seed))
                                        )
                                        n
                            in ()

                        Nothing ->
                            Debug.log
                                ("  Seed {seed}: did not reach target"
                                    |> String.replace "{seed}" (String.fromInt seed)
                                )
                                ()
            in
            ( k + 1, result :: acc )
    in
    List.foldl step ( 1, [] ) seeds
        |> Tuple.second
        |> List.reverse
        |> List.filterMap identity


{-| Find smallest fuzz such that runWithOptions(fuzz, seed) yields >= target failures.
Returns Nothing if even maxFuzz doesn't reach the target.
-}
minimalFuzzForSeed : Int -> Int -> Int -> Maybe Int
minimalFuzzForSeed target cap seed =
    let
        high =
            findUpperBound seed target cap
        failed =
            runCount seed high
    in
    if failed >= target then
        Just (binarySearch seed target 1 high)

    else
        Nothing


runCount : Int -> Int -> Int
runCount seed fuzz =
    Runner.String.runWithOptions fuzz (Random.initialSeed seed) EffectivenessTests.all
        |> .failed


{-| Exponential probe: 1, 2, 4, 8, ... until failed >= target or fuzz >= cap.
Returns the fuzz value (which is then used as the high bound for binary search).
-}
findUpperBound : Int -> Int -> Int -> Int
findUpperBound seed target cap =
    findUpperBoundHelp seed target 1 cap


findUpperBoundHelp : Int -> Int -> Int -> Int -> Int
findUpperBoundHelp seed target fuzz cap =
    let
        failed =
            runCount seed fuzz
    in
    if failed >= target then
        fuzz

    else if fuzz >= cap then
        fuzz

    else
        findUpperBoundHelp seed target (fuzz * 2) cap


{-| Smallest fuzz in [low, high] such that runCount seed fuzz >= target.
Invariant: runCount seed high >= target.
-}
binarySearch : Int -> Int -> Int -> Int -> Int
binarySearch seed target low high =
    if low >= high then
        low

    else
        let
            mid =
                (low + high) // 2

            failed =
                runCount seed mid
        in
        if failed >= target then
            binarySearch seed target low mid

        else
            binarySearch seed target (mid + 1) high


report : List Int -> List Int -> ()
report seeds values =
    let
        n =
            List.length seeds

        m =
            List.length values

        msg =
            if m == 0 then
                "No seed in {n} reached {target} failures (maxFuzz = {maxFuzz})."
                    |> String.replace "{n}" (String.fromInt n)
                    |> String.replace "{target}" (String.fromInt targetFailures)
                    |> String.replace "{maxFuzz}" (String.fromInt maxFuzz)

            else
                let
                    sorted =
                        List.sort values

                    minVal =
                        Maybe.withDefault 0 (List.minimum values)

                    maxVal =
                        Maybe.withDefault 0 (List.maximum values)

                    sum =
                        List.sum values

                    avg =
                        toFloat sum / toFloat m

                    median =
                        medianOfSorted sorted
                in
                """
{m}/{n} reached target
min = {min}
max = {max}
avg = {avg}
p50 = {median}

Finished!"""
                    |> String.replace "{target}" (String.fromInt targetFailures)
                    |> String.replace "{start}" (String.fromInt startSeed)
                    |> String.replace "{end}" (String.fromInt endSeed)
                    |> String.replace "{m}" (String.fromInt m)
                    |> String.replace "{n}" (String.fromInt n)
                    |> String.replace "{min}" (String.fromInt minVal)
                    |> String.replace "{max}" (String.fromInt maxVal)
                    |> String.replace "{avg}" (String.fromFloat (roundTo 1 avg))
                    |> String.replace "{median}" (String.fromInt median)
    in
    Debug.log msg ()


medianOfSorted : List Int -> Int
medianOfSorted sorted =
    case sorted of
        [] ->
            0

        [ x ] ->
            x

        _ ->
            let
                len =
                    List.length sorted

                half =
                    len // 2
            in
            if modBy 2 len == 1 then
                sorted |> List.drop half |> List.head |> Maybe.withDefault 0

            else
                let
                    a =
                        sorted |> List.drop (half - 1) |> List.head |> Maybe.withDefault 0

                    b =
                        sorted |> List.drop half |> List.head |> Maybe.withDefault 0
                in
                (a + b) // 2


roundTo : Int -> Float -> Float
roundTo decimals x =
    let
        p =
            toFloat (10 ^ decimals)
    in
    toFloat (round (x * p)) / p
