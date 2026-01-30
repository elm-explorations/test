module Test.Html.SelectorTests exposing (all)

{-| Tests for selectors
-}

import Fuzz exposing (..)
import Html
import Html.Attributes as Attr
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


all : Test
all =
    describe "Test.Html.Selector"
        [ bug13
        , textSelectors
        , exactTextSelectors
        , selectorAll
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
                    |> Query.has
                        [ Selector.text "Text1"
                        , Selector.text "Text2"
                        ]
        , test "the welcome <h1> says hello!" <|
            \() ->
                Html.div []
                    [ Html.h1
                        [ Attr.title "greeting"
                        , Attr.class "me"
                        ]
                        [ Html.text "Hello!" ]
                    ]
                    |> Query.fromHtml
                    |> Query.find [ Selector.attribute (Attr.title "greeting") ]
                    |> Query.has
                        [ Selector.text "Hello!"
                        , Selector.class "me"
                        ]
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
                    |> Query.has [ Selector.text str ]
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
                    |> Query.has (List.map Selector.text strings)
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
                    |> Query.has [ Selector.text str ]
        ]


nonemptyString : Fuzzer String
nonemptyString =
    stringOfLengthBetween 1 10


exactTextSelectors : Test
exactTextSelectors =
    describe "Selector.exactText"
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
                    |> Query.has [ Selector.exactText str ]
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
                    |> Query.has (List.map Selector.exactText strings)
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
                    |> Query.hasNot [ Selector.exactText str2 ]
        , test "Trimming is not happening" <|
            \() ->
                Html.div [] [ Html.text """
                    We like whitespace
                """ ]
                    |> Query.fromHtml
                    |> Query.hasNot [ Selector.exactText "We like whitespace" ]
        ]


selectorAll : Test
selectorAll =
    describe "Selector.all"
        [ test "passes if empty" <|
            \() ->
                Html.fieldset [ Attr.disabled False ]
                    [ Html.button [ Attr.disabled True ]
                        [ Html.text "Reply"
                        ]
                    ]
                    |> Query.fromHtml
                    |> Query.has [ Selector.all [] ]
        , test "passes if single selector matches" <|
            \() ->
                Html.fieldset [ Attr.disabled False ]
                    [ Html.button [ Attr.disabled True ]
                        [ Html.text "Reply"
                        ]
                    ]
                    |> Query.fromHtml
                    |> Query.has
                        [ Selector.all
                            [ Selector.tag "fieldset"
                            ]
                        ]
        , test "passes if all selectors match" <|
            \() ->
                Html.fieldset [ Attr.disabled False ]
                    [ Html.button [ Attr.disabled True ]
                        [ Html.text "Reply"
                        ]
                    ]
                    |> Query.fromHtml
                    |> Query.has
                        [ Selector.all
                            [ Selector.tag "fieldset"
                            , Selector.attribute (Attr.disabled False)
                            ]
                        ]
        , test "fails if some but not all selectors match (regression for #213)" <|
            \() ->
                Html.fieldset [ Attr.disabled False ]
                    [ Html.button [ Attr.disabled True ]
                        [ Html.text "Reply"
                        ]
                    ]
                    |> Query.fromHtml
                    |> Query.hasNot
                        [ Selector.all
                            [ Selector.tag "fieldset"
                            , Selector.attribute (Attr.disabled True)
                            ]
                        ]
        , test "fails if no selectors match" <|
            \() ->
                Html.fieldset [ Attr.disabled False ]
                    [ Html.button [ Attr.disabled True ]
                        [ Html.text "Reply"
                        ]
                    ]
                    |> Query.fromHtml
                    |> Query.hasNot
                        [ Selector.all
                            [ Selector.tag "strong"
                            , Selector.attribute (Attr.disabled True)
                            ]
                        ]
        ]
