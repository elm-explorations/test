module Test.Expectation exposing
    ( Expectation(..)
    , fail
    , withCoverageReport
    , withGiven
    )

import Test.Coverage exposing (CoverageReport(..))
import Test.Runner.Failure exposing (Reason)


type Expectation
    = Pass { coverageReport : CoverageReport }
    | Fail
        { given : Maybe String
        , description : String
        , reason : Reason
        , coverageReport : CoverageReport
        }


{-| Create a failure without specifying the given.
-}
fail : { description : String, reason : Reason } -> Expectation
fail { description, reason } =
    Fail
        { given = Nothing
        , description = description
        , reason = reason
        , coverageReport = NoCoverage
        }


{-| Set the given (fuzz test input) of an expectation.
-}
withGiven : String -> Expectation -> Expectation
withGiven newGiven expectation =
    case expectation of
        Fail failure ->
            Fail { failure | given = Just newGiven }

        Pass _ ->
            expectation


{-| Set the coverage report of an expectation.
-}
withCoverageReport : CoverageReport -> Expectation -> Expectation
withCoverageReport newCoverageReport expectation =
    case expectation of
        Fail failure ->
            Fail { failure | coverageReport = newCoverageReport }

        Pass pass ->
            Pass { pass | coverageReport = newCoverageReport }
