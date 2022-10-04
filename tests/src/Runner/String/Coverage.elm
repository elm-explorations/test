module Runner.String.Coverage exposing (report)

import Test.Coverage exposing (CoverageReport(..))


report : List String -> CoverageReport -> Maybe String
report testBreadcrumbs coverageReport =
    case coverageReport of
        NoCoverage ->
            Nothing

        CoverageToReport r ->
            let
                breadcrumbsPath =
                    testBreadcrumbs
                        |> List.reverse
                        |> String.join " > "
            in
            Just <| breadcrumbsPath ++ "\n" ++ Test.Coverage.coverageReportTable r

        CoverageCheckSucceeded _ ->
            Nothing

        CoverageCheckFailed _ ->
            {- Don't show it here: it's already included in the failure message.
               That way the Node runner will show it too.
            -}
            Nothing
