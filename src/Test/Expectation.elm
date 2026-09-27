module Test.Expectation exposing
    ( Expectation(..)
    , FailData
    , fail
    )

import Test.Distribution exposing (DistributionReport(..))
import Test.Runner.Failure exposing (Reason)


type Expectation
    = Pass DistributionReport
    | Fail
        { given : Maybe String
        , failData : FailData
        , distributionReport : DistributionReport
        }


{-| The data the `Expect` module can set for failing expectations.
-}
type alias FailData =
    { description : String
    , reason : Reason
    }


fail : FailData -> Expectation
fail failData =
    Fail
        { failData = failData
        , given = Nothing
        , distributionReport = NoDistribution ()
        }
