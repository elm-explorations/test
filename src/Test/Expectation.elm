module Test.Expectation ( Expectation(..)
    , fail
    , withDistributionReport
    , withGiven
    )
 where

import Test.Distribution (DistributionReport(..))
import Test.Distribution as Test.Distribution
import Test.Runner.Failure (Reason)
import Test.Runner.Failure as Test.Runner.Failure


data Expectation
    = Pass { distributionReport :: DistributionReport }
    | Fail
        { given :: Maybe String
        , description :: String
        , reason :: Reason
        , distributionReport :: DistributionReport
        }


{-| Create a failure without specifying the given.
-}
fail :: { description :: String, reason :: Reason } -> Expectation
fail { description, reason } =
    Fail
        { given : Nothing
        , description : description
        , reason : reason
        , distributionReport : NoDistribution
        }


{-| Set the given (fuzz test input) of an expectation.
-}
withGiven :: String -> Expectation -> Expectation
withGiven newGiven expectation =
    case expectation of
        Fail failure ->
            Fail ( failure { given = Just newGiven  })

        Pass _ ->
            expectation


{-| Set the distribution report of an expectation.
-}
withDistributionReport :: DistributionReport -> Expectation -> Expectation
withDistributionReport newDistributionReport expectation =
    case expectation of
        Fail failure ->
            Fail ( failure { distributionReport = newDistributionReport  })

        Pass pass ->
            Pass ( pass { distributionReport = newDistributionReport  })
