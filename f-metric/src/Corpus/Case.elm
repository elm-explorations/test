module Corpus.Case exposing (Case, Role(..), fails, shrinkOnly, violation)

{-| A single F-metric corpus entry: a fuzz test that is _known to fail_, because
a defect has been deliberately injected into the property.

For each case we measure how long the test system takes to uncover the defect,
and how good the counterexample it reports is. See `README.md`.


## Adding a case

Use [`fails`](#fails). Three things matter:

1.  The property must genuinely fail for _some_ inputs, otherwise the case is
    only ever recorded as "not detected" and tells us nothing.
2.  The defect should be reachable but not trivial. If every seed finds it on
    the first run, the case can't distinguish two generation strategies.
3.  `minima` should list every acceptable "best possible" counterexample, as
    rendered by `Debug.toString`. Leave it empty if you don't know them — you
    lose the counterexample-quality score for that case but nothing else.

Then run the tool and look at `med cases`. If it's 1 or 2, the very first values
the fuzzer produced already falsify the property, so the case says nothing about
how well generation searches — mark it [`shrinkOnly`](#shrinkOnly).

The easiest way to fill in `minima` is to leave it empty, run the tool once and
look at the `given` values it reports.

-}

import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (Test)


{-| What a case is actually able to measure.

`Search` cases need the fuzzer to work to find the defect, so their case counts
and times mean something. `ShrinkOnly` cases are falsified by the first value or
two; all their cost is simplification, so including them in an F aggregate just
dilutes it. They're still worth keeping for the `optimal` score — that's the only
place we measure simplification quality across many seeds rather than one — but
they're excluded from runs by default.

-}
type Role
    = Search
    | ShrinkOnly


type alias Case =
    { name : String
    , category : String
    , role : Role

    {- `Debug.toString` renderings of the counterexamples we'd consider optimal.
       Empty means "we don't know", which disables the quality score for this case.
    -}
    , minima : List String

    {- Overrides the harness-wide budget. Only for cases that are hopeless at
       the default budget; both sides of a comparison must use the same value.
    -}
    , budget : Maybe Int
    , test : Test
    }


{-| The marker we fail with, so that the harness can tell "the property was
violated, as intended" apart from "this case is broken" (an invalid fuzzer, too
many filtered values, a runtime exception, ...).
-}
violation : String
violation =
    "F-METRIC-VIOLATION"


{-| Build a corpus case out of a fuzzer and a property that it can falsify.
-}
fails :
    { name : String
    , category : String
    , minima : List String
    , budget : Maybe Int
    }
    -> Fuzzer a
    -> (a -> Bool)
    -> Case
fails config fuzzer property =
    { name = config.name
    , category = config.category
    , role = Search
    , minima = config.minima
    , budget = config.budget
    , test =
        Test.fuzz config.name fuzzer <|
            \value ->
                if property value then
                    Expect.pass

                else
                    Expect.fail violation
    }


{-| Mark a case as measuring only simplification, not search. See [`Role`](#Role).
-}
shrinkOnly : Case -> Case
shrinkOnly case_ =
    { case_ | role = ShrinkOnly }
