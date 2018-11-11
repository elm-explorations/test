module Descendant exposing (all)

-- TODO: rename to Test.Html.DescendantTest

import ElmHtml.InternalTypes exposing (ElmHtml(..))
import Expect exposing (Expectation)
import Html exposing (..)
import Html.Inert exposing (fromHtml, toElmHtml)
import Test exposing (..)
import Test.Html.Query as Query exposing (Single)
import Test.Runner


doesContain : Html msg -> Html msg -> Bool
doesContain potentialDescendant html =
    html
        |> Query.fromHtml
        |> Query.contains [ potentialDescendant ]
        |> expectationToIsPassing


all : Test
all =
    describe "Contains assertion"
        [ test "returns true if it contains the expected html once" <|
            \() ->
                div [] [ h1 [] [ text "foo" ] ]
                    |> doesContain (h1 [] [ text "foo" ])
                    |> Expect.equal True
        , test "returns true if it contains the expected html more than once" <|
            \() ->
                div []
                    [ h1 [] [ text "foo" ]
                    , h1 [] [ text "foo" ]
                    ]
                    |> doesContain (h1 [] [ text "foo" ])
                    |> Expect.equal True
        , test "return true if the node is a nested descendant" <|
            \() ->
                div []
                    [ div []
                        [ div [] [ h1 [] [ text "foo" ] ]
                        ]
                    ]
                    |> doesContain (h1 [] [ text "foo" ])
                    |> Expect.equal True
        , test "returns false if it does not contain the node" <|
            \() ->
                div [] [ h1 [] [ text "foo" ] ]
                    |> doesContain (img [] [])
                    |> Expect.equal False
        ]


expectationToIsPassing : Expectation -> Bool
expectationToIsPassing expectation =
    case Test.Runner.getFailureReason expectation of
        Nothing ->
            True

        Just _ ->
            False
