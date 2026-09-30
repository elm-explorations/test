port module Main exposing (main)

{-| Throughput benchmarks for elm-test.

Where `f-metric/` asks "is elm-test good at finding bugs", this asks "how fast
does it get through a test". The two answer different questions, and the F-metric
harness structurally cannot answer this one: its whole corpus is failing tests,
so it never measures the case that dominates a real suite -- a fuzz test that
passes and therefore runs its full budget.

That's also the case where exhaustive checking should pay off by stopping early:
`Fuzz.bool` given `--fuzz 100` has two possible inputs and ought to cost two
runs, not a hundred.

Headless rather than browser-based, so it runs in CI and so numbers can be
produced without a human clicking anything. That means it no longer uses
elm-explorations/benchmark, whose runner is a browser program; the statistics are
simple enough to do in `runner.mjs`.

Uses the public `Test.Runner` API. Its `run : () -> List Expectation` is a plain
function, which is what makes it benchmarkable -- `Test.RunnerV2.runFuzzTest`
returns a `Task`, and a Task can't be timed from inside Elm.

-}

import Array exposing (Array)
import Json.Decode as Decode
import Json.Encode as Encode
import Platform
import Random
import Snippets
import Test exposing (Test)
import Test.Runner


port toJs : Encode.Value -> Cmd msg


port fromJs : (Decode.Value -> msg) -> Sub msg


main : Program () () Msg
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update = update
        , subscriptions = \_ -> fromJs FromJs
        }


type Msg
    = FromJs Decode.Value


type Request
    = Manifest
    | Run RunParams


type alias RunParams =
    { index : Int
    , runs : Int
    , seed : Int
    , iterations : Int
    }


update : Msg -> () -> ( (), Cmd Msg )
update (FromJs raw) model =
    case Decode.decodeValue requestDecoder raw of
        Err error ->
            ( model
            , toJs
                (Encode.object
                    [ ( "tag", Encode.string "error" )
                    , ( "error", Encode.string (Decode.errorToString error) )
                    ]
                )
            )

        Ok Manifest ->
            ( model
            , toJs
                (Encode.object
                    [ ( "tag", Encode.string "manifest" )
                    , ( "benchmarks"
                      , Encode.list
                            (\( name, passes, _ ) ->
                                Encode.object
                                    [ ( "name", Encode.string name )

                                    {- runs/sec is only meaningful for a test that
                                       actually executes its whole budget. A failing
                                       test stops at the first value, so dividing by
                                       `runs` would overstate it by orders of
                                       magnitude.
                                    -}
                                    , ( "passes", Encode.bool passes )
                                    ]
                            )
                            benchmarks
                      )
                    ]
                )
            )

        Ok (Run params) ->
            case Array.get params.index (Array.fromList benchmarks) of
                Nothing ->
                    ( model
                    , toJs
                        (Encode.object
                            [ ( "tag", Encode.string "error" )
                            , ( "error", Encode.string "no benchmark at that index" )
                            ]
                        )
                    )

                Just ( _, _, test ) ->
                    ( model
                    , toJs
                        (Encode.object
                            [ ( "tag", Encode.string "done" )

                            {- The count exists to force the work. Without
                               consuming the expectations there'd be nothing
                               stopping the whole loop being optimized away.
                            -}
                            , ( "checksum", Encode.int (repeat params test) )
                            ]
                        )
                    )


requestDecoder : Decode.Decoder Request
requestDecoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "manifest" ->
                        Decode.succeed Manifest

                    "run" ->
                        Decode.map4 RunParams
                            (Decode.field "index" Decode.int)
                            (Decode.field "runs" Decode.int)
                            (Decode.field "seed" Decode.int)
                            (Decode.field "iterations" Decode.int)
                            |> Decode.map Run

                    _ ->
                        Decode.fail ("unknown tag: " ++ tag)
            )


{-| Run the whole test `iterations` times, returning the number of expectations
produced so the work can't be elided.

Each iteration rebuilds the runners from scratch, which is what a real runner
does per test, and keeps one iteration from warming state for the next.

-}
repeat : RunParams -> Test -> Int
repeat params test =
    repeatHelp params test params.iterations 0


repeatHelp : RunParams -> Test -> Int -> Int -> Int
repeatHelp params test remaining acc =
    if remaining <= 0 then
        acc

    else
        let
            expectations : Int
            expectations =
                case Test.Runner.fromTest params.runs (Random.initialSeed params.seed) test of
                    Test.Runner.Plain runners ->
                        List.foldl (\runner sum -> sum + List.length (runner.run ())) 0 runners

                    _ ->
                        0
        in
        repeatHelp params test (remaining - 1) (acc + expectations)



-- BENCHMARKS


{-| Name, and the test to run.

The `(passes)` cases are the interesting ones for throughput: they run the full
`runs` budget. The `simplify/` ones stop at the first value and then simplify, so
they measure simplification cost instead.

-}
benchmarks : List ( String, Bool, Test )
benchmarks =
    [ -- Small enough to enumerate exhaustively. If exhaustive checking lands,
      -- these should collapse to a couple of runs regardless of the budget.
      ( "small/unit", True, Snippets.unitPass )
    , ( "small/bool", True, Snippets.boolPass )
    , ( "small/order", True, Snippets.orderPass )
    , ( "small/intRange-0-20", True, Snippets.intRange0To20Pass )
    , ( "small/pair-bool", True, Snippets.pairBoolPass )
    , ( "small/maybe-bool", True, Snippets.maybeBoolPass )
    , ( "small/oneOfValues", True, Snippets.oneOfValuesPass )

    -- Mid-size: finite, but not trivially so. These decide where a budget
    -- boundary should sit.
    , ( "mid/intRange-0-1000", True, Snippets.intRange0To1000Pass )
    , ( "mid/pair-intRange-0-30", True, Snippets.pairIntRange0To30Pass )

    -- Unbounded. These must not regress: they're where an enumeration probe or
    -- deduplication bookkeeping is pure overhead.
    , ( "large/int", True, Snippets.intPass )
    , ( "large/float", True, Snippets.floatPass )
    , ( "large/string", True, Snippets.stringPass )
    , ( "large/list-int", True, Snippets.listIntPass )
    , ( "large/record-map5", True, Snippets.map5Pass )

    -- Rejection path.
    , ( "filter/even", True, Snippets.filterPass )

    -- Simplification cost, for contrast with the passing cases.
    , ( "simplify/int", False, Snippets.intFail )
    , ( "simplify/string", False, Snippets.stringFail )
    , ( "simplify/list-int", False, Snippets.listIntFail )
    ]
