module Test.Expectation exposing
    ( Expectation(..)
    , InvertedFailure
    , withDistributionReport
    , withGiven
    )

import Test.Distribution exposing (DistributionReport)
import Test.Runner.Failure exposing (Reason)


type Expectation
    = Pass
        { distributionReport : DistributionReport

        {- What to report if somebody inverts this pass with `Expect.not`.

           It's a function so that we don't pay for `Debug.toString`ing the
           compared values on the happy path - fuzz tests produce a lot of
           passing expectations.

           `Nothing` means "we have nothing useful to say about this pass",
           e.g. for `Expect.pass` itself.
        -}
        , ifInverted : Maybe (() -> InvertedFailure)
        }
    | Fail
        { given : Maybe String
        , description : String
        , reason : Reason
        , distributionReport : DistributionReport
        }


{-| The failure that `Expect.not` should report when it inverts a passing
expectation - everything except the `distributionReport`, which the `Pass`
itself owns (`withDistributionReport` can replace it after the fact, and we
don't want the inverted failure to report a stale one).

Keeping `given` here means inverting a failure and inverting it back gives you
the original failure, `given` included.

-}
type alias InvertedFailure =
    { given : Maybe String
    , description : String
    , reason : Reason
    }


{-| Set the given (fuzz test input) of an expectation.
-}
withGiven : String -> Expectation -> Expectation
withGiven newGiven expectation =
    case expectation of
        Fail failure ->
            Fail
                { given = Just newGiven
                , description = failure.description
                , reason = failure.reason
                , distributionReport = failure.distributionReport
                }

        Pass _ ->
            expectation


{-| Set the distribution report of an expectation.
-}
withDistributionReport : DistributionReport -> Expectation -> Expectation
withDistributionReport newDistributionReport expectation =
    case expectation of
        Fail failure ->
            Fail
                { given = failure.given
                , description = failure.description
                , reason = failure.reason
                , distributionReport = newDistributionReport
                }

        Pass passed ->
            Pass { passed | distributionReport = newDistributionReport }
