module Test.Html.SelectorTests exposing (all)

{-| Tests for selectors
-}

import Fuzz exposing (..)
import Html
import Html.Attributes as Attr
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)


all : Test
all =
    describe "Test.Html.Selector"
        [ bug13
        , textSelectors
        , textExactlySelectors
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
        [ fuzz3 (list string) string (list string) "Finds one result" <|
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
        , fuzz3 (list string) (list string) (list string) "Finds multiple results" <|
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
        , fuzz3 (list string) string (list string) "Finds a submatch" <|
            \before str after ->
                let
                    textNodes =
                        [ before, [ "hello" ++ str ++ "world" ], after ]
                            |> List.concat
                            |> List.map Html.text
                in
                Html.div [] textNodes
                    |> Query.fromHtml
                    |> Query.has [ text str ]
        ]


nonemptyString : Fuzzer String
nonemptyString =
    stringOfLengthBetween 1 10


textExactlySelectors : Test
textExactlySelectors =
    describe "Selector.textExactly"
        [ fuzz3 (list string) string (list string) "Finds one result" <|
            \before str after ->
                let
                    textNodes =
                        [ before, [ str ], after ]
                            |> List.concat
                            |> List.map Html.text
                in
                Html.div [] textNodes
                    |> Query.fromHtml
                    |> Query.has [ textExactly str ]
        , fuzz3 (list string) (list string) (list string) "Finds multiple results" <|
            \before strings after ->
                let
                    textNodes =
                        [ before, strings, after ]
                            |> List.concat
                            |> List.map Html.text
                in
                Html.div [] textNodes
                    |> Query.fromHtml
                    |> Query.has (List.map textExactly strings)
        , fuzz3 (list nonemptyString) nonemptyString (list nonemptyString) "Doesn't find a submatch" <|
            \before str after ->
                let
                    str1 =
                        if List.member str before then
                            str ++ "_"

                        else
                            str

                    str2 =
                        if List.member str1 after then
                            str1 ++ "_"

                        else
                            str1

                    textNodes =
                        [ before, [ "hello" ++ str2 ++ "world" ], after ]
                            |> List.concat
                            |> List.map Html.text
                in
                Html.div [] textNodes
                    |> Query.fromHtml
                    |> Query.hasNot [ textExactly str2 ]
        , test "Trimming is not happening" <|
            \() ->
                Html.div [] [ Html.text """
                    We like whitespace
                """ ]
                    |> Query.fromHtml
                    |> Query.hasNot [ textExactly "We like whitespace" ]
        ]
