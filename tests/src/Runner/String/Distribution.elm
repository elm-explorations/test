module Runner.String.Distribution (report) where

import Dict (Dict)
import Dict as Dict
import Expect (Expectation)
import Expect as Expect
import Set (Set)
import Set as Set
import Test.Distribution (DistributionReport(..))
import Test.Distribution as Test.Distribution
import Test.Runner as Test.Runner


report :: List String -> DistributionReport -> Maybe String
report testBreadcrumbs distributionReport =
    case distributionReport of
        NoDistribution ->
            Nothing

        DistributionToReport r ->
            let
                breadcrumbsPath =
                    testBreadcrumbs
                        |> List.reverse
                        |> String.join " > "
            in
            Just <| breadcrumbsPath <> "\n" <> Test.Distribution.distributionReportTable r

        DistributionCheckSucceeded _ ->
            Nothing

        DistributionCheckFailed _ ->
            {- Don't show it here: it's already included in the failure message.
               That way the Node runner will show it too.
            -}
            Nothing
