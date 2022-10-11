## Releases

| Version                                                                              | Notes                                                                                                                                                     |
| ------------------------------------------------------------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------------------------- |
| [**2.0.0**](https://github.com/elm-explorations/test/tree/2.0.0)                     | Reimplements fuzzing+shrinking, adds fuzzer distribution reporting. Most notably readds `Fuzz.andThen`. See ["Changes in 2.0.0"](#changes-in-200) |
| [**1.2.2**](https://github.com/elm-explorations/test/tree/1.2.2)                     | Fixes a crash in `Test.Html` when the HTML contains nested `Html.Lazy` nodes. [#78](https://github.com/elm-explorations/test/issues/78)                   |
| [**1.2.1**](https://github.com/elm-explorations/test/tree/1.2.1)                     | Many small documentation fixes.  Improve error messages when failing to simulate an event.                                                                |
| [**1.2.0**](https://github.com/elm-explorations/test/tree/1.2.0)                     | Add HTML tests. [#41](https://github.com/elm-explorations/test/pull/41)                                                                                   |
| [**1.0.0**](https://github.com/elm-explorations/test/tree/1.0.0)                     | Update for Elm 0.19. Remove `Fuzz.andThen`, `Fuzz.conditional`, and `Test.Runner.getFailure`. Fail on equating floats to encourage checks with tolerance. `Test.Runner.fuzz` now returns a `Result`. |
| renamed from **elm-community/elm-test** (below) to **elm-explorations/test** (above) |                                                                                                                                                           |
| [**4.0.0**](https://github.com/elm-community/elm-test/tree/4.0.0)                    | Add `only`, `skip`, `todo`; change `Fuzz.frequency` to fail rather than crash on bad input, disallow tests with blank or duplicate descriptions.          |
| [**3.1.0**](https://github.com/elm-community/elm-test/tree/3.1.0)                    | Add `Expect.all`                                                                                                                                          |
| [**3.0.0**](https://github.com/elm-community/elm-test/tree/3.0.0)                    | Update for Elm 0.18; switch the argument order of `Fuzz.andMap`.                                                                                          |
| [**2.1.0**](https://github.com/elm-community/elm-test/tree/2.1.0)                    | Switch to rose trees for `Fuzz.andThen`, other API additions.                                                                                             |
| [**2.0.0**](https://github.com/elm-community/elm-test/tree/2.0.0)                    | Scratch-rewrite to project-fuzzball                                                                                                                       |
| [**1.0.0**](https://github.com/elm-community/elm-test/tree/1.0.0)                    | ElmTest initial release                                                                                                                                   |
## Changes in 2.0.0

The changes can be grouped into these categories:

1. [Fuzzing and shrinking reimplementation](#1-fuzzing-and-shrinking-reimplementation) (re-adding `Fuzz.andThen` etc.)
2. [`Test.Distribution`](#2-testdistribution): fuzzer distribution reporting
3. [`Test.Html.Event`](#3-testhtmlevent-additions) additions
4. [`Expect.true` and `Expect.false` removal](#4-expecttrue-and-expectfalse-removal)
5. [`Test.Runner.Failure.format` removal](#5-testrunnerfailureformat-removal)
6. [Fuzzer behaviour changes](#6-fuzzer-behaviour-changes)

### 1. Fuzzing and shrinking reimplementation

Fuzzing and shrinking has been reimplemented: the rose tree approach has been
replaced with the "internal shrinking" approach found in the Python test
library [Hypothesis](https://github.com/HypothesisWorks/hypothesis).

In short, shrinking is now done on the PRNG history instead of on the generated
values themselves. This is hidden from the user: the `Shrink` module has now
been removed.

This new approach allows us to reintroduce `Fuzz.andThen` and remove
`Fuzz.custom`: in case you were forced to use `Fuzz.custom` and a `Random`
generator, you'll now be able to express this logic with `Fuzz` alone.

We've also taken the opportunity to expand the `Fuzz` module API with functions
that you'd normally reach into `*-extra` packages for: `sequence`, `traverse`,
`listOfLength`, `intAtLeast` etc., and some quality of life helpers like
`examples` and `labelExamples`:

```elm
> import Fuzz
> Fuzz.examples 5 (Fuzz.intRange 1 10)
[6,7,7,5,4] : List Int
```

Float fuzzers will now generate `NaN` and infinities by default (use
`Fuzz.niceFloat` instead if you don't want these values) and shrink towards
simple human-readable fractions like `0.5` instead of small floats like
`1.1102230246251567e-15`.

Full list of changes:

- Generic helpers
  - :heavy_plus_sign: `andThen : (a -> Fuzzer b) -> Fuzzer a -> Fuzzer b`
  - :heavy_plus_sign: `filter : (a -> Bool) -> Fuzzer a -> Fuzzer a`
  - :heavy_plus_sign: `lazy : (() -> Fuzzer a) -> Fuzzer a`
  - :heavy_plus_sign: `map6`
  - :heavy_plus_sign: `map7`
  - :heavy_plus_sign: `map8`
  - :heavy_plus_sign: `sequence : List (Fuzzer a) -> Fuzzer (List a)`
  - :heavy_plus_sign: `traverse : (a -> Fuzzer b) -> List a -> Fuzzer (List b)`
  - :heavy_plus_sign: `shuffledList : List a -> Fuzzer (List a)`

- `oneOf` helpers
  - :heavy_plus_sign: `oneOfValues : List a -> Fuzzer a`
  - :heavy_plus_sign: `frequencyValues : List ( Float, a ) -> Fuzzer a`

- REPL helpers
  - :heavy_plus_sign: `examples : Int -> Fuzzer a -> List a`
  - :heavy_plus_sign: `labelExamples : Int -> List ( String, a -> Bool ) -> Fuzzer a -> List ( List String, Maybe a )`

- Tuples
  - :pencil: `tuple` changed into `pair : Fuzzer a -> Fuzzer b -> Fuzzer ( a, b )`
  - :pencil: `tuple3` changed into `triple : Fuzzer a -> Fuzzer b -> Fuzzer c -> Fuzzer ( a, b, c )`

- Bools
  - :heavy_plus_sign: `weightedBool : Float -> Fuzzer Bool`

- Ints
  - :heavy_plus_sign: `intAtLeast : Int -> Fuzzer Int`
  - :heavy_plus_sign: `intAtMost : Int -> Fuzzer Int`
  - :heavy_plus_sign: `uniformInt : Int -> Fuzzer Int`

- Floats
  - :heavy_plus_sign: `floatAtLeast : Float -> Fuzzer Float`
  - :heavy_plus_sign: `floatAtMost : Float -> Fuzzer Float`
  - :heavy_plus_sign: `niceFloat : Fuzzer Float`

- ASCII
  - :heavy_plus_sign: `asciiChar : Fuzzer Char`
  - :heavy_plus_sign: `asciiString : Fuzzer String`

- Collections of a given length
  - :heavy_plus_sign: `listOfLength : Int -> Fuzzer a -> Fuzzer (List a)`
  - :heavy_plus_sign: `listOfLengthBetween : Int -> Int -> Fuzzer a -> Fuzzer (List a)`
  - :heavy_plus_sign: `stringOfLength : Int -> Fuzzer String`
  - :heavy_plus_sign: `stringOfLengthBetween : Int -> Int -> Fuzzer String`
  - :heavy_plus_sign: `asciiStringOfLength : Int -> Fuzzer String`
  - :heavy_plus_sign: `asciiStringOfLengthBetween : Int -> Int -> Fuzzer String`

- Escape hatches
  - :heavy_minus_sign: `custom : Generator a -> Shrinker a -> Fuzzer a`
  - :heavy_plus_sign: (discouraged escape hatch) `fromGenerator : Generator a -> Fuzzer a`

### 2. `Test.Distribution`

You can now report or enforce the value distribution of your fuzzers with
`Test.reportDistribution` and `Test.expectDistribution`.

For more information on this technique, we recommend watching the talk
["Building on developers' intuitions to create effective property-based
tests"](https://www.youtube.com/watch?v=NcJOiQlzlXQ) by John Hughes.

Plug these functions into `Test.fuzzWith`:

```elm
  Test.fuzzWith
      { runs = 10000
      , distribution =
          Test.reportDistribution
              [ ( "low", \n -> n == 1 )
              , ( "high", \n -> n == 20 )
              , ( "in between", \n -> n > 1 && n < 20 )
              , ( "outside", \n -> n < 1 || n > 20 )
              ]
      }
      (Fuzz.intRange 1 20)
      "Example for Test.reportDistribution"
      (\n -> Expect.pass)
```

Reporting will never change the outcome of a test, but will always output a
distribution report table next to the other test results:

```
↓ Distribution.ReportDistributionPassing
✓ Example for Test.reportDistribution

    Distribution report:
    ====================
      in between:  90.5%  (9046x)  ███████████████████████████░░░
      low:          4.9%   (485x)  █░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
      high:         4.7%   (469x)  █░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
      outside:        0%     (0x)  ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
```

On the other hand, the function `Test.expectDistribution` will _make sure_ the fuzzers have the distribution you expect:

```elm
    Test.fuzzWith
        { runs = 10000
        , distribution =
            Test.expectDistribution
                [ -- Expecting the number 1 to be generated at least 10% of the time, which is too much!
                  ( Test.Distribution.atLeast 10, "low", \n -> n == 1 )
                , ( Test.Distribution.atLeast 4, "high", \n -> n == 20 )
                , ( Test.Distribution.atLeast 80, "in between", \n -> n > 1 && n < 20 )
                , ( Test.Distribution.zero, "outside", \n -> n < 1 || n > 20 )
                , ( Test.Distribution.moreThanZero, "one", \n -> n == 1 )
                ]
        }
        (Fuzz.intRange 1 20)
        "Example for Test.expectDistribution (with insufficient distribution)"
        (\n -> Expect.pass)
```

Resulting in a failure (although the fuzz test itself does pass):

```
↓ Distribution.ExpectDistributionFailingDistribution
✗ Example for Test.expectDistribution (with insufficient distribution)

    Distribution report:
    ====================
      in between:  90%  (9004x)  ███████████████████████████░░░
      high:         5%   (498x)  █░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
      low:          5%   (498x)  █░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
      one:          5%   (498x)  █░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
      outside:      0%     (0x)  ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

    Combinations (included in the above base counts):
      low, one:     5%   (498x)  █░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

    Distribution of label "low" was insufficient:
      expected:  10.000%
      got:       4.980%.

    (Generated 10000 values.)
```

Note that to reduce the flakiness of such assertion (particularly when your
expected percentage is very close to the actual percentage, eg. 4.9% expected
and 5% actual), the test runner can occasionally run more tests than specified
in `FuzzOptions.runs`.

> :book: This check is implemented using the [Wilson score
interval](https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Wilson_score_interval)
and is tuned to accept one flaky result roughly every 10^9 runs.

### 3. `Test.Html.Event` additions

These new expectations have been added:

```elm
expectNotPreventDefault  : Test.Html.Event.Event msg -> Expectation
expectNotStopPropagation : Test.Html.Event.Event msg -> Expectation
expectPreventDefault     : Test.Html.Event.Event msg -> Expectation
expectStopPropagation    : Test.Html.Event.Event msg -> Expectation
```

### 4. `Expect.true` and `Expect.false` removal

These functions were encouraging an easy but not idiomatic way to create an
`Expectation` in situations where other `Expect.*` functions would have been a
better choice.

You can still achieve the same result with:

```elm
-- BEFORE
foo
  |> Expect.true "string saying what went wrong"

-- AFTER
foo
  |> Expect.equal True
  |> Expect.onFail "string saying what went wrong"
```

### 5. `Test.Runner.Failure.format` removal

The function `Test.Runner.Failure.format` was deprecated since 1.2.0; with
2.0.0 we're now fully removing it.

### 6. Fuzzer behaviour changes

String fuzzers now generate Unicode text (including emojis and combining marks). Use `asciiString` or `asciiChar` if you only want the ASCII subset.

```elm
> Fuzz.examples 3 Fuzz.string
["y\n\t@]̂z❤B","O🔥","̈4🌈&"] : List String
```

Reported equality failures now show strings both verbatim and with Unicode characters escaped if needed, to better show things like non-breaking spaces.

```
↓ Expectations
↓ Expect.equal on unicode strings should show pretty output
✗ ascii
           ▼        ▼▼       ▼▼
    "\u{1f63b}\u{1f640}\u{1f47b}" (same string but with unicode characters escaped)
     ▼
    "😻🙀👻"
    ╵
    │ |> Expect.equal
    ╷
    "🙀👻😻🙈"
       ▲▲
    "\u{1f640}\u{1f47b}\u{1f63b}\u{1f648}" (same string but with unicode characters escaped)
           ▲▲▲▲▲▲▲▲▲▲        ▲▲      ▲ ▲
```
