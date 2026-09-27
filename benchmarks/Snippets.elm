module Snippets exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Test exposing (Test, fuzz)


intPass : Test
intPass =
    fuzz "(passes) int" Fuzz.int <|
        \_ ->
            Expect.pass


intFail : Test
intFail =
    fuzz "(fails) int" Fuzz.int <|
        \numbers ->
            Expect.fail "Failed"


intRangePass : Test
intRangePass =
    fuzz "(passes) intRange" (Fuzz.intRange 10 100) <|
        \_ ->
            Expect.pass


intRangeFail : Test
intRangeFail =
    fuzz "(fails) intRange" (Fuzz.intRange 10 100) <|
        \numbers ->
            Expect.fail "Failed"


stringPass : Test
stringPass =
    fuzz "(passes) string" Fuzz.string <|
        \_ ->
            Expect.pass


stringFail : Test
stringFail =
    fuzz "(fails) string" Fuzz.string <|
        \numbers ->
            Expect.fail "Failed"


floatPass : Test
floatPass =
    fuzz "(passes) float" Fuzz.float <|
        \_ ->
            Expect.pass


floatFail : Test
floatFail =
    fuzz "(fails) float" Fuzz.float <|
        \numbers ->
            Expect.fail "Failed"


boolPass : Test
boolPass =
    fuzz "(passes) bool" Fuzz.bool <|
        \_ ->
            Expect.pass


boolFail : Test
boolFail =
    fuzz "(fails) bool" Fuzz.bool <|
        \numbers ->
            Expect.fail "Failed"


charPass : Test
charPass =
    fuzz "(passes) char" Fuzz.char <|
        \_ ->
            Expect.pass


charFail : Test
charFail =
    fuzz "(fails) char" Fuzz.char <|
        \numbers ->
            Expect.fail "Failed"


listIntPass : Test
listIntPass =
    fuzz "(passes) list of int" (Fuzz.list Fuzz.int) <|
        \_ ->
            Expect.pass


listIntFail : Test
listIntFail =
    fuzz "(fails) list of int" (Fuzz.list Fuzz.int) <|
        {- The empty list is the first value the list simplifier will try.
           If we immediately fail on that example than we're not doing a lot of simplifying.
        -}
        Expect.notEqual []


maybeIntPass : Test
maybeIntPass =
    fuzz "(passes) maybe of int" (Fuzz.maybe Fuzz.int) <|
        \_ ->
            Expect.pass


maybeIntFail : Test
maybeIntFail =
    fuzz "(fails) maybe of int" (Fuzz.maybe Fuzz.int) <|
        \numbers ->
            Expect.fail "Failed"


resultPass : Test
resultPass =
    fuzz "(passes) result of string and int" (Fuzz.result Fuzz.string Fuzz.int) <|
        \_ ->
            Expect.pass


resultFail : Test
resultFail =
    fuzz "(fails) result of string and int" (Fuzz.result Fuzz.string Fuzz.int) <|
        \numbers ->
            Expect.fail "Failed"


mapPass : Test
mapPass =
    fuzz "(passes) map" even <|
        \_ -> Expect.pass


mapFail : Test
mapFail =
    fuzz "(fails) map" even <|
        \_ -> Expect.fail "Failed"


andMapPass : Test
andMapPass =
    fuzz "(passes) andMap" person <|
        \_ -> Expect.pass


andMapFail : Test
andMapFail =
    fuzz "(fails) andMap" person <|
        \_ -> Expect.fail "Failed"


map5Pass : Test
map5Pass =
    fuzz "(passes) map5" person2 <|
        \_ -> Expect.pass


map5Fail : Test
map5Fail =
    fuzz "(fails) map5" person2 <|
        \_ -> Expect.fail "Failed"


type alias Person =
    { firstName : String
    , lastName : String
    , age : Int
    , nationality : String
    , height : Float
    }


person : Fuzzer Person
person =
    Fuzz.map Person Fuzz.string
        |> Fuzz.andMap Fuzz.string
        |> Fuzz.andMap Fuzz.int
        |> Fuzz.andMap Fuzz.string
        |> Fuzz.andMap Fuzz.float


person2 : Fuzzer Person
person2 =
    Fuzz.map5 Person
        Fuzz.string
        Fuzz.string
        Fuzz.int
        Fuzz.string
        Fuzz.float


even : Fuzzer Int
even =
    Fuzz.map ((*) 2) Fuzz.int


sequence : List (Fuzzer a) -> Fuzzer (List a)
sequence fuzzers =
    List.foldl
        (Fuzz.map2 (::))
        (Fuzz.constant [])
        fuzzers
