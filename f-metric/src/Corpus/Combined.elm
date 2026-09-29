module Corpus.Combined exposing (cases)

{-| Cases where part of the input domain is tiny and part of it is huge, and the
defect lives in the huge part.

This is the shape that makes exhaustive checking and duplicate rejection worth
having, and it's the motivating example from
<https://github.com/elm-explorations/test/issues/188>: given

    Fuzz.oneOf
        [ Fuzz.map Err Fuzz.string
        , Fuzz.map Ok Fuzz.bool
        ]

`oneOf` splits its runs evenly, so half of them go on re-testing `Ok True` and
`Ok False` over and over. A defect confined to the `Err` branch is therefore
found at half the rate it could be, and getting the `Ok` side out of the way
should roughly halve F.

The three cases below differ in exactly the way that separates the candidate
implementations, so the numbers say _which_ mechanism paid off:

  - `combined/open-payload` — the `Err` payload is `Fuzz.string`, so the choice
    tree is **infinite**. All-or-nothing enumeration can't help: it hits its node
    budget and falls back to sampling. Only per-run duplicate rejection (#207) or
    a per-subtree hybrid can win here.
  - `combined/finite-payload` — the `Err` payload is a bounded uniform int, so
    the whole tree is **finite and enumerable**. Plain exhaustive checking should
    win outright, and by more than 2x: it never repeats an input at all.
  - `combined/wide-easy-branch` — like `finite-payload`, but the easy branch has
    201 leaves instead of 2. This is the case where leaf-level dedup and
    subtree-level reasoning should come apart. Dedup has to _observe_ all 201
    distinct `Ok` runs before it stops wasting work, which by coupon-collector is
    about `201 * H(201)` ~ 1200 `Ok` draws; an enumerator that tracks exhausted
    subtrees knows the branch is finished as soon as it has covered it.

All three share the same `Err` payload size, so their baseline F is comparable
and a change can be read across the row.

-}

import Corpus.Case as Case exposing (Case)
import Fuzz exposing (Fuzzer)


cases : List Case
cases =
    [ openPayload
    , finitePayload
    , wideEasyBranch
    ]


{-| A uniform integer in `0..1023`.

Built from two sub-255 draws so it stays uniform: `Fuzz.intRange` buckets above
255 and the odds of hitting a particular value stop being analytic. With 1024
equally likely payloads and `oneOf` giving the `Err` branch half the runs, the
per-run detection probability is `0.5 / 1024`, so F should sit around 2048 --
comfortably inside a 10 000 budget, and far enough above the noise that a 2x
improvement is unmistakable.

-}
uniform1024 : Fuzzer Int
uniform1024 =
    Fuzz.map2 (\hi lo -> hi * 32 + lo)
        (Fuzz.intRange 0 31)
        (Fuzz.intRange 0 31)


{-| The needle in the `Err` branch. Arbitrary, but fixed: what matters is that
it's one value out of 1024.
-}
errNeedle : Int
errNeedle =
    999


{-| The issue's example as written: `Result String Bool`, with the defect in the
`Err` branch.

The payload is `Fuzz.string`, so the tree is infinite and exhaustive checking has
nothing to bite on. Requiring two separate rare characters keeps F in the same
range as the finite cases: `Fuzz.char` draws printable ASCII half the time and
uniformly over 95 characters when it does, so a given character appears in a
given string with probability around 2.6%, and needing two of them lands the
per-`Err` rate near 1e-3.

-}
openPayload : Case
openPayload =
    Case.fails
        { name = "combined/open-payload"
        , category = "combined"
        , minima = []
        , budget = Nothing
        }
        (Fuzz.oneOf
            [ Fuzz.map Err Fuzz.string
            , Fuzz.map Ok Fuzz.bool
            ]
        )
        (\result ->
            case result of
                Ok _ ->
                    True

                Err payload ->
                    not (String.contains "z" payload && String.contains "q" payload)
        )


{-| The same shape, but with a bounded payload so the entire choice tree is
finite: 1024 `Err` leaves plus 2 `Ok` leaves. Exhaustive checking should find the
defect within about 1026 generated values, guaranteed, rather than ~2048 on
average.
-}
finitePayload : Case
finitePayload =
    Case.fails
        { name = "combined/finite-payload"
        , category = "combined"
        , minima = []
        , budget = Nothing
        }
        (Fuzz.oneOf
            [ Fuzz.map Err uniform1024
            , Fuzz.map Ok Fuzz.bool
            ]
        )
        (\result ->
            case result of
                Ok _ ->
                    True

                Err payload ->
                    payload /= errNeedle
        )


{-| An easy branch with 201 leaves rather than 2, to separate leaf-level dedup
from subtree-level exhaustion. See the module docs.
-}
wideEasyBranch : Case
wideEasyBranch =
    Case.fails
        { name = "combined/wide-easy-branch"
        , category = "combined"
        , minima = []
        , budget = Nothing
        }
        (Fuzz.oneOf
            [ Fuzz.map Err uniform1024
            , Fuzz.map Ok (Fuzz.intRange 0 200)
            ]
        )
        (\result ->
            case result of
                Ok _ ->
                    True

                Err payload ->
                    payload /= errNeedle
        )
