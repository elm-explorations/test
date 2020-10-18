module DebugLogTests exposing (all)

import Expect exposing (FloatingPointTolerance(..))
import Fuzz exposing (..)
import Helpers exposing (..)
import Test exposing (..)


all : Test
all =
    describe "Meta Debug.log"
        [ fuzz fuzzer "debug log for 1-arg fuzzer" <|
            \meta a ->
                meta.log (Debug.toString { desc = "fuzz1-meta-log", a = a }) <|
                    (a |> Expect.notEqual 5)
        , fuzz2 fuzzer fuzzer "debug log for 2-arg fuzzer" <|
            \meta a b ->
                meta.log (Debug.toString { desc = "fuzz2-meta-log", a = a, b = b }) <|
                    (( a, b ) |> Expect.notEqual ( 2, 5 ))
        , fuzz3 fuzzer fuzzer fuzzer "debug log for 3-arg fuzzer" <|
            \meta a b c ->
                meta.log (Debug.toString { desc = "fuzz3-meta-log", a = a, b = b, c = c }) <|
                    (( a, b, c ) |> Expect.notEqual ( 2, 3, 5 ))
        ]

fuzzer = intRange 2 5