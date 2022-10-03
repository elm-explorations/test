module Test.Html.SelectorTests exposing (all)

{-| Tests for selectors
-}

import Fuzz
import Html
import Html.Attributes as Attr
import Test exposing (Test, describe, fuzz3, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, class, text)


all : Test
all =
    describe "Test.Html.Selector"
        [ bug13
        , textSelectors
        ]


{-| <https://github.com/eeue56/elm-html-test/issues/13>
-}
bug13 : Test
bug13 =
    describe "Reproducing bug #13"
        [ test "Using Selector.text twice checks for both." <|
            \() ->
                Html.div []
                    [ Html.text "Text1"
                    , Html.text "Text2"
                    ]
                    |> Query.fromHtml
                    |> Query.has [ text "Text1", text "Text2" ]
        , test "the welcome <h1> says hello!" <|
            \() ->
                Html.div []
                    [ Html.h1 [ Attr.title "greeting", Attr.class "me" ] [ Html.text "Hello!" ] ]
                    |> Query.fromHtml
                    |> Query.find [ attribute (Attr.title "greeting") ]
                    |> Query.has [ text "Hello!", class "me" ]
        ]


textSelectors : Test
textSelectors =
    describe "Selector.text"
        [ fuzz3
            (Fuzz.list Fuzz.string)
            Fuzz.string
            (Fuzz.list Fuzz.string)
            "Finds one result"
          <|
            \before str after ->
                let
                    textNodes =
                        [ before, [ str ], after ]
                            |> List.concat
                            |> List.map Html.text
                in
                Html.div [] textNodes
                    |> Query.fromHtml
                    |> Query.has [ text str ]
        , fuzz3
            (Fuzz.list Fuzz.string)
            (Fuzz.list Fuzz.string)
            (Fuzz.list Fuzz.string)
            "Finds multiple results"
          <|
            \before strings after ->
                let
                    textNodes =
                        [ before, strings, after ]
                            |> List.concat
                            |> List.map Html.text
                in
                Html.div [] textNodes
                    |> Query.fromHtml
                    |> Query.has (List.map text strings)
        ]
