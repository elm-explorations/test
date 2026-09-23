module Test.Html.SelectorTests exposing (all)

{-| Tests for selectors
-}

import Fuzz exposing (..)
import Html
import Html.Attributes as Attr
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector exposing (..)


all : Test
all =
    describe "Test.Html.Selector"
        [ bug13
        , textSelectors
        , exactTextSelectors
        , selectorAllTests
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
        [ fuzz3 "Finds one result" (list string) string (list string) <|
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
        , fuzz3 "Finds multiple results" (list string) (list string) (list string) <|
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
        , fuzz3 "Finds a submatch" (list string) string (list string) <|
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


exactTextSelectors : Test
exactTextSelectors =
    describe "Selector.exactText"
        [ fuzz3 "Finds one result" (list string) string (list string) <|
            \before str after ->
                let
                    textNodes =
                        [ before, [ str ], after ]
                            |> List.concat
                            |> List.map Html.text
                in
                Html.div [] textNodes
                    |> Query.fromHtml
                    |> Query.has [ exactText str ]
        , fuzz3 "Finds multiple results" (list string) (list string) (list string) <|
            \before strings after ->
                let
                    textNodes =
                        [ before, strings, after ]
                            |> List.concat
                            |> List.map Html.text
                in
                Html.div [] textNodes
                    |> Query.fromHtml
                    |> Query.has (List.map exactText strings)
        , fuzz3 "Doesn't find a submatch" (list nonemptyString) nonemptyString (list nonemptyString) <|
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
                    |> Query.hasNot [ exactText str2 ]
        , test "Trimming is not happening" <|
            \() ->
                Html.div [] [ Html.text """
                    We like whitespace
                """ ]
                    |> Query.fromHtml
                    |> Query.hasNot [ exactText "We like whitespace" ]
        ]


{-| <https://github.com/elm-explorations/test/issues/213>
<https://github.com/elm-explorations/test/issues/214>

`Selector.all` must require all of its selectors to match the same element.

-}
selectorAllTests : Test
selectorAllTests =
    let
        html =
            Html.fieldset [ Attr.disabled False ]
                [ Html.button [ Attr.disabled True ]
                    [ Html.text "Reply"
                    ]
                ]
    in
    describe "Selector.all"
        [ test "passes with an empty list" <|
            \() ->
                html
                    |> Query.fromHtml
                    |> Query.has [ Selector.all [] ]
        , test "passes if a single selector matches" <|
            \() ->
                html
                    |> Query.fromHtml
                    |> Query.has [ Selector.all [ tag "fieldset" ] ]
        , test "passes if every selector matches the same element" <|
            \() ->
                html
                    |> Query.fromHtml
                    |> Query.has
                        [ Selector.all
                            [ tag "fieldset"
                            , attribute (Attr.disabled False)
                            ]
                        ]
        , test "fails if the selectors are only satisfied by different elements (regression for #213)" <|
            \() ->
                html
                    |> Query.fromHtml
                    |> Query.hasNot
                        [ Selector.all
                            [ tag "fieldset"
                            , attribute (Attr.disabled True)
                            ]
                        ]
        , test "fails if no element matches" <|
            \() ->
                html
                    |> Query.fromHtml
                    |> Query.hasNot
                        [ Selector.all
                            [ tag "strong"
                            , attribute (Attr.disabled True)
                            ]
                        ]
        , test "still finds text among the same element's descendants" <|
            \() ->
                html
                    |> Query.fromHtml
                    |> Query.has
                        [ Selector.all
                            [ tag "button"
                            , text "Reply"
                            ]
                        ]
        , test "Query.find returns the element that matched, not a descendant" <|
            \() ->
                html
                    |> Query.fromHtml
                    |> Query.find
                        [ Selector.all
                            [ tag "button"
                            , attribute (Attr.disabled True)
                            ]
                        ]
                    |> Query.has [ text "Reply" ]
        ]
