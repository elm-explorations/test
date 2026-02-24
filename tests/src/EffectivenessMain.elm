module EffectivenessMain exposing (main)

{-| Effectiveness test runner: finds the minimal fuzz count per seed
to reach a target number of failed test cases, then reports min/max/avg/median.

$ cd tests
$ elm make src/EffectivenessMain.elm --output effectiveness-runner.js
$ node effectiveness-runner.js

-}

import EffectivenessTests
import Platform
import Random
import Runner.String exposing (Summary)


config : { targetFailures : Int, totalSeeds : Int, maxFuzz : Int }
config =
    { targetFailures = 10
    , totalSeeds = 100
    , maxFuzz = 1000000
    }


main : Program () () ()
main =
    let
        _ =
            run ()
    in
    Platform.worker
        { init = \() -> ( (), Cmd.none )
        , update = \_ s -> ( s, Cmd.none )
        , subscriptions = \() -> Sub.none
        }


run : () -> ()
run () =
    let
        _ =
            Debug.log
                ("Target {target} failures, seeds 1..{total}"
                    |> String.replace "{target}" (String.fromInt config.targetFailures)
                    |> String.replace "{total}" (String.fromInt config.totalSeeds)
                )
                ()
    in
    case report (measure ()) of
        Nothing ->
            Debug.log
                ("No seed in {total} reached {target} failures (maxFuzz = {maxFuzz})."
                    |> String.replace "{total}" (String.fromInt config.totalSeeds)
                    |> String.replace "{target}" (String.fromInt config.targetFailures)
                    |> String.replace "{maxFuzz}" (String.fromInt config.maxFuzz)
                )
                ()

        Just { reached, min, max, avg, median } ->
            Debug.log
                ("""
{reached}/{total} reached target
min = {min}
max = {max}
avg = {avg}
p50 = {median}

Finished!"""
                    |> String.replace "{reached}" (String.fromInt reached)
                    |> String.replace "{total}" (String.fromInt config.totalSeeds)
                    |> String.replace "{min}" (String.fromInt min)
                    |> String.replace "{max}" (String.fromInt max)
                    |> String.replace "{avg}" (String.fromFloat (roundTo 1 avg))
                    |> String.replace "{median}" (String.fromInt median)
                )
                ()


measure : () -> List Int
measure () =
    let
        digits =
            String.length (String.fromInt config.totalSeeds)

        step seed acc =
            case minimalFuzzForSeed config.targetFailures config.maxFuzz seed of
                Just n ->
                    let
                        _ =
                            Debug.log
                                ("  Seed {seed}: minimal fuzz"
                                    |> String.replace "{seed}" (String.padLeft digits ' ' (String.fromInt seed))
                                )
                                n
                    in
                    n :: acc

                Nothing ->
                    let
                        _ =
                            Debug.log
                                ("  Seed {seed}: did not reach target"
                                    |> String.replace "{seed}" (String.fromInt seed)
                                )
                                ()
                    in
                    acc
    in
    List.foldl step [] (List.range 1 config.totalSeeds)


{-| Find smallest fuzz such that runWithOptions(fuzz, seed) yields >= target failures.
Returns Nothing if even maxFuzz doesn't reach the target.
-}
minimalFuzzForSeed : Int -> Int -> Int -> Maybe Int
minimalFuzzForSeed target cap seed =
    let
        ( low, high ) =
            findBounds seed target cap

        failed =
            runCount seed high
    in
    if failed >= target then
        Just (binarySearch seed target low high)

    else
        Nothing


runCount : Int -> Int -> Int
runCount seed fuzz =
    Runner.String.runWithOptions fuzz (Random.initialSeed seed) EffectivenessTests.all
        |> .failed


{-| Exponential probe: 1, 2, 4, 8, ... until failed >= target or fuzz >= cap.
Returns (low, high) where low is the last probe that did not reach target (or 1)
and high is the first fuzz that reached target; used as bounds for binary search.
-}
findBounds : Int -> Int -> Int -> ( Int, Int )
findBounds seed target cap =
    findBoundsHelp seed target 1 cap


findBoundsHelp : Int -> Int -> Int -> Int -> ( Int, Int )
findBoundsHelp seed target fuzz cap =
    let
        failed =
            runCount seed fuzz
    in
    if failed >= target || fuzz >= cap then
        ( max 1 (fuzz // 2), fuzz )

    else
        findBoundsHelp seed target (fuzz * 2) cap


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


report : List Int -> Maybe { reached : Int, min : Int, max : Int, avg : Float, median : Int }
report values =
    let
        reached =
            List.length values
    in
    if reached == 0 then
        Nothing

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
                toFloat sum / toFloat reached

            median =
                medianOfSorted sorted
        in
        Just
            { reached = reached
            , min = minVal
            , max = maxVal
            , avg = avg
            , median = median
            }


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

                dropped =
                    sorted |> List.drop (half - 1)
            in
            if modBy 2 len == 1 then
                dropped |> List.drop 1 |> List.head |> Maybe.withDefault 0

            else
                let
                    a =
                        dropped |> List.head |> Maybe.withDefault 0

                    b =
                        dropped |> List.drop 1 |> List.head |> Maybe.withDefault 0
                in
                (a + b) // 2


roundTo : Int -> Float -> Float
roundTo decimals x =
    let
        p =
            toFloat (10 ^ decimals)
    in
    toFloat (round (x * p)) / p
