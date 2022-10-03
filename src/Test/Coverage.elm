module Test.Coverage exposing
    ( ExpectedCoverage, atLeast, zero, moreThanZero
    , CoverageReport(..), coverageReportTable
    )

{-|


## Coverage

@docs ExpectedCoverage, atLeast, zero, moreThanZero
@docs CoverageReport, coverageReportTable

-}

import Dict exposing (Dict)
import Test.Coverage.Internal
import Test.Runner.Coverage


{-| Your input distribution requirement for the fuzzer used in a test.

For example, "this test shouldn't ever receive strings of length < 3 as an input"
or "at least 30% of the test input trees should be balanced".

-}
type alias ExpectedCoverage =
    Test.Coverage.Internal.ExpectedCoverage


{-| A requirement that a given value class should never happen in a given test.
-}
zero : ExpectedCoverage
zero =
    Test.Coverage.Internal.Zero


{-| A requirement that a given value class should happen at least once in a given
test.
-}
moreThanZero : ExpectedCoverage
moreThanZero =
    Test.Coverage.Internal.MoreThanZero


{-| A requirement that a given value class should happen at least N% of the time
in a given test.

The example below says that at least 30% of the fuzz test inputs should be
multiples of 3.

    fuzzWith
        { runs = 10000
        , coverage =
            expectCoverage
                [ ( atLeast 30, "multiple of 3", \n -> (n |> modBy 3) == 0 )
                ]
        }

-}
atLeast : Float -> ExpectedCoverage
atLeast =
    Test.Coverage.Internal.AtLeast


{-| A result of a coverage check.

Get it from your `Expectation` with `Test.Runner.getCoverageReport`.

-}
type CoverageReport
    = NoCoverage
    | CoverageToReport
        { coverageCount : Dict (List String) Int
        , runsElapsed : Int
        }
    | CoverageCheckSucceeded
        { coverageCount : Dict (List String) Int
        , runsElapsed : Int
        }
    | CoverageCheckFailed
        { coverageCount : Dict (List String) Int
        , runsElapsed : Int
        , badLabel : String
        , badLabelPercentage : Float
        , {- Would be great to return ExpectedCoverage here but it's defined (and
             used) in an internal module. The only way from this dependency cycle
             I can see involves exposing the constructors and having a duplicate
             definition of this type + conversion functions between them.
             ~janiczek
          -}
          expectedCoverage : String
        }


{-| Prettyprints the record inside `CoverageReport` into a table with histograms.
-}
coverageReportTable :
    { a
        | runsElapsed : Int
        , coverageCount : Dict (List String) Int
    }
    -> String
coverageReportTable r =
    Test.Runner.Coverage.formatTable r
