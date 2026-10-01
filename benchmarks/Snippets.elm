module Snippets exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Set
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



{- Passing tests over small, exhaustible domains. These are the ones exhaustive
   checking should be able to finish early: the whole input space fits in far
   fewer values than a typical `runs` budget.
-}


unitPass : Test
unitPass =
    fuzz "(passes) unit" Fuzz.unit <|
        \_ -> Expect.pass


orderPass : Test
orderPass =
    fuzz "(passes) order" Fuzz.order <|
        \_ -> Expect.pass


intRange0To20Pass : Test
intRange0To20Pass =
    fuzz "(passes) intRange 0 20" (Fuzz.intRange 0 20) <|
        \_ -> Expect.pass


pairBoolPass : Test
pairBoolPass =
    fuzz "(passes) pair of bools" (Fuzz.pair Fuzz.bool Fuzz.bool) <|
        \_ -> Expect.pass


maybeBoolPass : Test
maybeBoolPass =
    fuzz "(passes) maybe bool" (Fuzz.maybe Fuzz.bool) <|
        \_ -> Expect.pass


oneOfValuesPass : Test
oneOfValuesPass =
    fuzz "(passes) oneOfValues" (Fuzz.oneOfValues [ 1, 2, 3, 4, 5 ]) <|
        \_ -> Expect.pass



{- Finite, but big enough that exhausting it is a real decision rather than a
   freebie.
-}


intRange0To1000Pass : Test
intRange0To1000Pass =
    fuzz "(passes) intRange 0 1000" (Fuzz.intRange 0 1000) <|
        \_ -> Expect.pass


pairIntRange0To30Pass : Test
pairIntRange0To30Pass =
    fuzz "(passes) pair of intRange 0 30"
        (Fuzz.pair (Fuzz.intRange 0 30) (Fuzz.intRange 0 30))
    <|
        \_ -> Expect.pass



{- Exercises the rejection path, which deduplication and enumeration both have
   to handle. Kept generous: `Fuzz.filter` gives up after 16 consecutive
   rejections and fails the test, and over thousands of runs even a mildly
   selective predicate hits that.
-}


filterPass : Test
filterPass =
    fuzz "(passes) filtered intRange"
        (Fuzz.intRange 0 100 |> Fuzz.filter (\n -> modBy 4 n /= 0))
    <|
        \_ -> Expect.pass



{- Varying how much work the *test body* does, holding the fuzzer fixed.

   The overhead of coverage tracking is largely per-run and fixed, so a heavier
   body should dilute it. Early termination works the other way: skipping a run
   skips its body too, so the heavier the body the bigger the saving. These pairs
   measure both effects, which is what a real test suite with substantial
   assertions would exercise.
-}


{-| An assertion with real work in it: round-trip the list through a Dict and
compare, which allocates and compares structures rather than checking a tag.
-}
heavyAssertion : List Int -> Expect.Expectation
heavyAssertion xs =
    xs
        |> List.map (\x -> ( x, String.fromInt x ))
        |> Dict.fromList
        |> Dict.toList
        |> List.map Tuple.first
        |> Expect.equal (xs |> Set.fromList |> Set.toList)


listIntHeavyPass : Test
listIntHeavyPass =
    fuzz "(passes) list of int, heavy assertion" (Fuzz.list Fuzz.int) heavyAssertion


boolHeavyPass : Test
boolHeavyPass =
    fuzz "(passes) bool, heavy assertion" Fuzz.bool <|
        \b ->
            heavyAssertion (List.range 0 60 |> List.filter (\n -> modBy 2 n == 0 || b))


pairBoolHeavyPass : Test
pairBoolHeavyPass =
    fuzz "(passes) pair of bools, heavy assertion" (Fuzz.pair Fuzz.bool Fuzz.bool) <|
        \( a, b ) ->
            heavyAssertion (List.range 0 60 |> List.filter (\n -> modBy 2 n == 0 || a || b))


stringHeavyPass : Test
stringHeavyPass =
    fuzz "(passes) string, heavy assertion" Fuzz.string <|
        \s ->
            heavyAssertion (String.toList s |> List.map Char.toCode)
