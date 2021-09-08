module Main exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner as Runner
import Expect exposing (Expectation)
import Random.Pcg
import Snippets
import Test.Internal exposing (Test(..))


main : Runner.BenchmarkProgram
main =
    Runner.program suite


suite : Benchmark
suite =
    describe "Fuzz"
        [ describe "int"
            [ benchmark "generating" (benchTest Snippets.intPass)
            , benchmark "simplifying" (benchTest Snippets.intFail)
            ]
        , describe "intRange"
            [ benchmark "generating" (benchTest Snippets.intRangePass)
            , benchmark "simplifying" (benchTest Snippets.intRangeFail)
            ]
        , describe "string"
            [ benchmark "generating" (benchTest Snippets.stringPass)
            , benchmark "simplifying" (benchTest Snippets.stringFail)
            ]
        , describe "float"
            [ benchmark "generating" (benchTest Snippets.floatPass)
            , benchmark "simplifying" (benchTest Snippets.floatFail)
            ]
        , describe "bool"
            [ benchmark "generating" (benchTest Snippets.boolPass)
            , benchmark "simplifying" (benchTest Snippets.boolFail)
            ]
        , describe "char"
            [ benchmark "generating" (benchTest Snippets.charPass)
            , benchmark "simplifying" (benchTest Snippets.charFail)
            ]
        , describe "list of int"
            [ benchmark "generating" (benchTest Snippets.listIntPass)
            , benchmark "simplifying" (benchTest Snippets.listIntFail)
            ]
        , describe "maybe of int"
            [ benchmark "generating" (benchTest Snippets.maybeIntPass)
            , benchmark "simplifying" (benchTest Snippets.maybeIntFail)
            ]
        , describe "result of string and int"
            [ benchmark "generating" (benchTest Snippets.resultPass)
            , benchmark "simplifying" (benchTest Snippets.resultFail)
            ]
        , describe "map"
            [ benchmark "generating" (benchTest Snippets.mapPass)
            , benchmark "simplifying" (benchTest Snippets.mapFail)
            ]
        , describe "andMap"
            [ benchmark "generating" (benchTest Snippets.andMapPass)
            , benchmark "simplifying" (benchTest Snippets.andMapFail)
            ]
        , describe "map5"
            [ benchmark "generating" (benchTest Snippets.map5Pass)
            , benchmark "simplifying" (benchTest Snippets.map5Fail)
            ]
        , describe "andThen"
            [ benchmark "generating" (benchTest Snippets.andThenPass)
            , benchmark "simplifying" (benchTest Snippets.andThenFail)
            ]
        , describe "conditional"
            [ benchmark "generating" (benchTest Snippets.conditionalPass)
            , benchmark "simplifying" (benchTest Snippets.conditionalFail)
            ]
        ]


benchTest : Test -> (() -> List Expectation)
benchTest test =
    case test of
        Test fn ->
            \_ -> fn (Random.Pcg.initialSeed 0) 10

        Labeled _ test ->
            benchTest test

        test ->
            Debug.crash <| "No support for benchmarking this type of test: " ++ toString test
