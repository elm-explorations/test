module Test.Expectation exposing
    ( Expectation(..)
    , FailData
    , FuzzTestExpectation(..)
    , fail
    , fromFuzzTestExpectation
    )

import RandomRun exposing (RandomRun)
import Test.Distribution exposing (DistributionReport(..))
import Test.Runner.Failure exposing (Reason)


type Expectation
    = -- `Pass` is not supposed to contain anything, but to avoid a breaking change
      -- we have to be able to store a `DistributionReport` as well.
      Pass DistributionReport
    | Fail BreakingChangeWorkaround


{-| This is supposed to _only_ contain `FailData`, but to avoid a breaking change
we have to be able to store data from `FuzzTestFail` as well.
-}
type alias BreakingChangeWorkaround =
    { failData : FailData
    , given : Maybe String
    , distributionReport : DistributionReport
    }


type alias FailData =
    { description : String
    , reason : Reason
    }


type FuzzTestExpectation
    = FuzzTestPass DistributionReport
    | FuzzTestFail
        { given : Maybe String
        , randomRun : RandomRun
        , description : String
        , reason : Reason
        , distributionReport : DistributionReport

        -- This function runs the fuzzer and the fuzz test again,
        -- with the input that caused this specific failure, and
        -- throws away the result. This is used in `Test.RunnerV2.runFuzzTest`
        -- to capture `Debug.log`s only from the execution that caused
        -- the failure, drastically reducing noise.
        , rerunFailure : () -> ()
        }


fail : FailData -> Expectation
fail failData =
    Fail
        { failData = failData
        , given = Nothing
        , distributionReport = NoDistribution
        }


{-| Due to backwards compatibility, we are forced to do this type conversion,
without losing data – see `BreakingChangeWorkaround`.
-}
fromFuzzTestExpectation : FuzzTestExpectation -> Expectation
fromFuzzTestExpectation fuzzTestExpectation =
    case fuzzTestExpectation of
        FuzzTestPass distributionReport ->
            Pass distributionReport

        FuzzTestFail record ->
            Fail
                { failData =
                    { description = record.description
                    , reason = record.reason
                    }
                , given = record.given
                , distributionReport = record.distributionReport
                }
