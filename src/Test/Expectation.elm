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
        , ifInverted : Maybe (() -> InvertedFailure)
        }
    | Fail
        { given : Maybe String
        , description : String
        , reason : Reason
        , distributionReport : DistributionReport
        }


{-| The failure that `Expect.not` should report when it inverts a passing
expectation.
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
