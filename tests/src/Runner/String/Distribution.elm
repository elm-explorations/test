module Runner.String.Distribution exposing (report)

import Test.Distribution exposing (DistributionReport(..))


report : List String -> DistributionReport -> Maybe String
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
            Just <| breadcrumbsPath ++ "\n" ++ Test.Distribution.distributionReportTable r

        DistributionCheckSucceeded _ ->
            Nothing

        DistributionCheckFailed _ ->
            {- Don't show it here: it's already included in the failure message.
               That way the Node runner will show it too.
            -}
            Nothing
