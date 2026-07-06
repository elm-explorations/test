module EffectivenessMain exposing (main)

{-| Effectiveness test runner: finds the minimal fuzz count per seed
to reach a target number of failed test cases, then reports min/max/avg/median.

Accepts flags: totalSeeds, multiplier, addend (seeds = i*multiplier+addend for i=0,1,... ≤ totalSeeds).
Use the Node script to pass flags from the CLI:

  $ cd tests
  $ elm make src/EffectivenessMain.elm --output effectiveness-runner.js
  $ node run-effectiveness.js [totalSeeds] [multiplier] [addend]

Defaults: 1000, 1, 0. For parallel runs use run-effectiveness-parallel.sh <workers> <totalSeeds>.

-}

import EffectivenessTests
import Json.Decode as Decode
import Platform
import Random
import Runner.String exposing (Summary)


type alias RunConfig =
    { totalSeeds : Int
    , multiplier : Int
    , addend : Int
    , targetFailures : Int
    , maxFuzz : Int
    }


defaultConfig : RunConfig
defaultConfig =
    { totalSeeds = 1000
    , multiplier = 1
    , addend = 0
    , targetFailures = 10
    , maxFuzz = 1000000
    }


decodeFlags : Decode.Decoder RunConfig
decodeFlags =
    Decode.map5 RunConfig
        (Decode.field "totalSeeds" Decode.int |> Decode.maybe |> Decode.map (Maybe.withDefault defaultConfig.totalSeeds))
        (Decode.field "multiplier" Decode.int |> Decode.maybe |> Decode.map (Maybe.withDefault defaultConfig.multiplier))
        (Decode.field "addend" Decode.int |> Decode.maybe |> Decode.map (Maybe.withDefault defaultConfig.addend))
        (Decode.succeed defaultConfig.targetFailures)
        (Decode.succeed defaultConfig.maxFuzz)


decodeFlagsWithDefaults : Decode.Decoder RunConfig
decodeFlagsWithDefaults =
    Decode.oneOf
        [ decodeFlags
        , Decode.succeed defaultConfig
        ]


{-| Seeds for this slice: i*multiplier+addend for i=0,1,... while <= totalSeeds.
-}
seedList : Int -> Int -> Int -> List Int
seedList totalSeeds multiplier addend =
    let
        k =
            (totalSeeds - addend) // multiplier
    in
    if addend > totalSeeds || k < 0 then
        []

    else
        List.range 0 k |> List.map (\i -> addend + i * multiplier)


main : Program Decode.Value () ()
main =
    Platform.worker
        { init = \flagsValue ->
            let
                cfg =
                    case Decode.decodeValue decodeFlagsWithDefaults flagsValue of
                        Ok c ->
                            c

                        Err _ ->
                            defaultConfig

                _ =
                    run cfg
            in
            ( (), Cmd.none )
        , update = \_ s -> ( s, Cmd.none )
        , subscriptions = \() -> Sub.none
        }


run : RunConfig -> ()
run cfg =
    let
        seeds =
            seedList cfg.totalSeeds cfg.multiplier cfg.addend

        _ =
            Debug.log
                ("Target {target} failures, seeds (addend {addend}): {count} seeds"
                    |> String.replace "{target}" (String.fromInt cfg.targetFailures)
                    |> String.replace "{addend}" (String.fromInt cfg.addend)
                    |> String.replace "{count}" (String.fromInt (List.length seeds))
                )
                ()
    in
    case report (List.length seeds) (measure cfg seeds) of
        Nothing ->
            Debug.log
                ("No seed in slice reached {target} failures (maxFuzz = {maxFuzz})."
                    |> String.replace "{target}" (String.fromInt cfg.targetFailures)
                    |> String.replace "{maxFuzz}" (String.fromInt cfg.maxFuzz)
                )
                ()

        Just { reached, min, max, avg, median } ->
            Debug.log
                ("""
{reached}/{total} reached target (addend {addend})
min = {min}
max = {max}
avg = {avg}
p50 = {median}

Finished!"""
                    |> String.replace "{reached}" (String.fromInt reached)
                    |> String.replace "{total}" (String.fromInt (List.length seeds))
                    |> String.replace "{addend}" (String.fromInt cfg.addend)
                    |> String.replace "{min}" (String.fromInt min)
                    |> String.replace "{max}" (String.fromInt max)
                    |> String.replace "{avg}" (String.fromFloat (roundTo 1 avg))
                    |> String.replace "{median}" (String.fromInt median)
                )
                ()


measure : RunConfig -> List Int -> List Int
measure cfg seeds =
    let
        digits =
            String.length (String.fromInt cfg.totalSeeds)

        step seed acc =
            case minimalFuzzForSeed cfg.targetFailures cfg.maxFuzz seed of
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
    List.foldl step [] seeds


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


report : Int -> List Int -> Maybe { reached : Int, min : Int, max : Int, avg : Float, median : Int }
report total values =
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
