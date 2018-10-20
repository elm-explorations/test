module Queries exposing (all)

import Expect
import Fuzz
import Html exposing (Html, a, div, footer, header, li, section, span, ul)
import Html.Attributes as Attr exposing (href)
import Html.Lazy as Lazy
import Test exposing (..)
import Test.Html.Query as Query exposing (Single)
import Test.Html.Selector exposing (..)


all : Test
all =
    describe "Queries"
        [ htmlTests
        , lazyTests
        ]


htmlTests : Test
htmlTests =
    describe "Html" <|
        List.map (\toTest -> toTest (Query.fromHtml sampleHtml)) testers


lazyTests : Test
lazyTests =
    describe "lazy Html" <|
        List.map (\toTest -> toTest (Query.fromHtml sampleLazyHtml)) testers


testers : List (Single msg -> Test)
testers =
    [ testFindAll
    , testKeep
    , testFind
    , testRoot
    , testFirst
    , testIndex
    , testChildren
    , testContaining
    ]


testRoot : Single msg -> Test
testRoot output =
    describe "root query without find or findAll"
        [ describe "finds itself" <|
            [ test "sees it's a <section class='root'>" <|
                \() ->
                    output
                        |> Expect.all
                            [ Query.has [ class "root" ]
                            , Query.has [ tag "section" ]
                            ]
            , test "recognizes its exact className" <|
                \() ->
                    output
                        |> Query.has [ exactClassName "root" ]
            , test "recognizes its class by classes" <|
                \() ->
                    output
                        |> Query.has [ classes [ "root" ] ]
            , test "recognizes its style by a single css property" <|
                \() ->
                    output
                        |> Query.has [ style [ ( "color", "red" ) ] ]
            , test "recognizes its style by multiple css properties" <|
                \() ->
                    output
                        |> Query.has [ style [ ( "color", "red" ), ( "background", "purple" ) ] ]
            , test "recognizes its style does not include a css property" <|
                \() ->
                    output
                        |> Query.hasNot [ style [ ( "color", "green" ) ] ]
            , test "recognizes if is has a specific descendant" <|
                \() ->
                    output
                        |> Query.contains [ someView "Such a title !" ]
            ]
        ]


testFind : Single msg -> Test
testFind output =
    describe "Query.find []"
        [ describe "finds the one child" <|
            [ test "sees it's a <div class='container'>" <|
                \() ->
                    output
                        |> Query.find []
                        |> Expect.all
                            [ Query.has [ class "container" ]
                            , Query.has [ tag "div" ]
                            ]
            , test "recognizes its exact className" <|
                \() ->
                    output
                        |> Query.find []
                        |> Query.has [ exactClassName "container" ]
            , test "recognizes its class by classes" <|
                \() ->
                    output
                        |> Query.find []
                        |> Query.has [ classes [ "container" ] ]
            , test "recognizes its style by style list" <|
                \() ->
                    output
                        |> Query.has [ style [ ( "color", "blue" ) ] ]
            , test "recognizes if is has a specific descendant" <|
                \() ->
                    output
                        |> Query.find []
                        |> Query.contains [ someView "Such a title !" ]
            ]
        ]


testFindAll : Single msg -> Test
testFindAll output =
    describe "Query.findAll []"
        [ describe "finds the one child" <|
            [ test "and only the one child" <|
                \() ->
                    output
                        |> Query.findAll []
                        |> Query.count (Expect.equal 1)
            , test "sees it's a <div class='container'>" <|
                \() ->
                    output
                        |> Query.findAll []
                        |> Expect.all
                            [ Query.each (Query.has [ class "container" ])
                            , Query.each (Query.has [ tag "div" ])
                            ]
            , test "recognizes its exact className" <|
                \() ->
                    output
                        |> Query.findAll []
                        |> Query.each (Query.has [ exactClassName "container" ])
            , test "recognizes its class by classes" <|
                \() ->
                    output
                        |> Query.findAll []
                        |> Query.each (Query.has [ classes [ "container" ] ])
            ]
        , describe "finds multiple descendants"
            [ test "with tag selectors that return one match at the start" <|
                \() ->
                    output
                        |> Query.findAll [ tag "header" ]
                        |> Query.count (Expect.equal 1)
            , test "with tag selectors that return multiple matches" <|
                \() ->
                    output
                        |> Query.findAll [ tag "section" ]
                        |> Query.count (Expect.equal 2)
            , test "with tag selectors that return one match at the end" <|
                \() ->
                    output
                        |> Query.find [ tag "footer" ]
                        |> Query.has [ text "this is the footer" ]
            , test "sees the nested div" <|
                \() ->
                    output
                        |> Query.findAll [ tag "div" ]
                        |> Query.count (Expect.equal 2)
            ]
        ]


testKeep : Single msg -> Test
testKeep output =
    describe "Query.keep"
        [ test "only keep a subsect of a result" <|
            \() ->
                output
                    |> Query.findAll [ tag "section" ]
                    |> Query.keep (tag "ul")
                    |> Query.keep (class "list-item")
                    |> Expect.all
                        [ Query.each (Query.has [ tag "li" ])
                        , Query.first >> Query.has [ text "first item" ]
                        ]
        , test "keep from the second section as well" <|
            \() ->
                output
                    |> Query.findAll [ tag "section" ]
                    |> Query.keep (class "nested-div")
                    |> Query.first
                    |> Query.has [ text "boring section" ]
        , test "keep elements from both matches" <|
            \() ->
                output
                    |> Query.findAll [ tag "section" ]
                    |> Query.keep (class "tooltip-questions")
                    |> Query.count (Expect.equal 2)
        ]


testFirst : Single msg -> Test
testFirst output =
    describe "Query.first"
        [ describe "finds the one child" <|
            [ test "sees it's a <div class='container'>" <|
                \() ->
                    output
                        |> Query.findAll []
                        |> Query.first
                        |> Query.has [ tag "div", class "container" ]
            ]
        ]


testIndex : Single msg -> Test
testIndex output =
    describe "Query.index"
        [ describe "only 1 element"
            [ test "index -1 matches" <|
                \() ->
                    output
                        |> Query.findAll []
                        |> Query.index -1
                        |> Query.has [ tag "div", class "container" ]
            , test "index 0 matches" <|
                \() ->
                    output
                        |> Query.findAll []
                        |> Query.index 0
                        |> Query.has [ tag "div", class "container" ]
            , test "index -2 too low" <|
                \() ->
                    output
                        |> Query.findAll []
                        |> Query.index -2
                        |> Query.hasNot [ tag "div" ]
            , test "index 1 too high" <|
                \() ->
                    output
                        |> Query.findAll []
                        |> Query.index 1
                        |> Query.hasNot [ tag "div" ]
            ]
        , describe "3 element"
            [ test "index 0 matches" <|
                \() ->
                    output
                        |> Query.findAll [ tag "a" ]
                        |> Query.index 0
                        |> Query.has [ tag "a", text "home" ]
            , test "index 1 matches" <|
                \() ->
                    output
                        |> Query.findAll [ tag "a" ]
                        |> Query.index 1
                        |> Query.has [ tag "a", text "examples" ]
            , test "index 2 matches" <|
                \() ->
                    output
                        |> Query.findAll [ tag "a" ]
                        |> Query.index 2
                        |> Query.has [ tag "a", text "docs" ]
            , test "index -3 matches" <|
                \() ->
                    output
                        |> Query.findAll [ tag "a" ]
                        |> Query.index -3
                        |> Query.has [ tag "a", text "home" ]
            , test "index -2 matches" <|
                \() ->
                    output
                        |> Query.findAll [ tag "a" ]
                        |> Query.index -2
                        |> Query.has [ tag "a", text "examples" ]
            , test "index -1 matches" <|
                \() ->
                    output
                        |> Query.findAll [ tag "a" ]
                        |> Query.index -1
                        |> Query.has [ tag "a", text "docs" ]
            , test "index -4  too low" <|
                \() ->
                    output
                        |> Query.findAll [ tag "a" ]
                        |> Query.index -4
                        |> Query.hasNot [ tag "a" ]
            , test "index 3 too high" <|
                \() ->
                    output
                        |> Query.findAll [ tag "a" ]
                        |> Query.index 3
                        |> Query.hasNot [ tag "a" ]
            ]
        ]


testChildren : Single msg -> Test
testChildren output =
    describe "Query.children"
        [ describe "on the root" <|
            [ test "sees the root has one child" <|
                \() ->
                    output
                        |> Query.children []
                        |> Expect.all
                            [ Query.count (Expect.equal 1)
                            , Query.each (Query.hasNot [ class "root" ])
                            ]
            , test "doesn't see the nested div" <|
                \() ->
                    output
                        |> Query.children [ class "nested-div" ]
                        |> Query.count (Expect.equal 0)
            , test "only children which match the selector get returned" <|
                \() ->
                    output
                        |> Query.find [ class "some-list" ]
                        |> Query.children [ class "selected" ]
                        |> Query.count (Expect.equal 1)
            ]
        ]


testContaining : Single msg -> Test
testContaining output =
    describe "Selector.containing"
        [ test "when it's a child" <|
            \() ->
                output
                    |> Query.findAll
                        [ tag "button"
                        , containing [ text "click me" ]
                        ]
                    |> Expect.all
                        [ Query.count (Expect.equal 1)
                        , Query.first >> Query.has [ class "super-button" ]
                        ]
        , test "when it's a grandchild" <|
            \() ->
                output
                    |> Query.findAll
                        [ tag "header"
                        , containing [ text "docs" ]
                        ]
                    |> Query.count (Expect.equal 1)
        , test "when it matches more than one element" <|
            \() ->
                output
                    |> Query.findAll
                        [ tag "section"
                        , containing [ text "?" ]
                        ]
                    |> Query.count (Expect.equal 2)
        ]


sampleHtml : Html msg
sampleHtml =
    section [ Attr.class "root", Attr.style [ ( "color", "red" ), ( "background", "purple" ), ( "font-weight", "bold" ) ] ]
        [ div [ Attr.class "container", Attr.style [ ( "color", "blue" ) ] ]
            [ header [ Attr.class "funky themed", Attr.id "heading" ]
                [ a [ href "http://elm-lang.org" ] [ Html.text "home" ]
                , a [ href "http://elm-lang.org/examples" ] [ Html.text "examples" ]
                , a [ href "http://elm-lang.org/docs" ] [ Html.text "docs" ]
                ]
            , someView "Such a title !"
            , section [ Attr.class "funky themed", Attr.id "section" ]
                [ ul [ Attr.class "some-list" ]
                    [ li [ Attr.class "list-item themed" ] [ Html.text "first item" ]
                    , li [ Attr.class "list-item themed" ] [ Html.text "second item" ]
                    , li [ Attr.class "list-item themed selected" ] [ Html.text "third item" ]
                    , li [ Attr.class "list-item themed" ] [ Html.text "fourth item" ]
                    , span [ Attr.class "tooltip-questions" ] [ Html.text "?" ]
                    ]
                ]
            , section []
                [ div [ Attr.class "nested-div" ] [ Html.text "boring section" ]
                , Html.button [ Attr.class "super-button" ] [ Html.text "click me" ]
                , Html.button [ Attr.class "other-button" ] [ Html.text "the other button" ]
                , span [ Attr.class "tooltip-questions" ] [ Html.text "?" ]
                ]
            , footer []
                [ Html.text "this is the footer"
                , span [ Attr.class "tooltip-questions" ] [ Html.text "?" ]
                ]
            ]
        ]


sampleLazyHtml : Html msg
sampleLazyHtml =
    section [ Attr.class "root", Attr.style [ ( "color", "red" ), ( "background", "purple" ), ( "font-weight", "bold" ) ] ]
        [ div [ Attr.class "container", Attr.style [ ( "color", "blue" ) ] ]
            [ header [ Attr.class "funky themed", Attr.id "heading" ]
                [ Lazy.lazy (\str -> a [ href "http://elm-lang.org" ] [ Html.text str ]) "home"
                , Lazy.lazy (\str -> a [ href "http://elm-lang.org/examples" ] [ Html.text str ]) "examples"
                , Lazy.lazy (\str -> a [ href "http://elm-lang.org/docs" ] [ Html.text str ]) "docs"
                ]
            , someView "Such a title !"
            , section [ Attr.class "funky themed", Attr.id "section" ]
                [ ul [ Attr.class "some-list" ]
                    [ Lazy.lazy (\str -> li [ Attr.class "list-item themed" ] [ Html.text str ]) "first item"
                    , Lazy.lazy (\str -> li [ Attr.class "list-item themed" ] [ Html.text str ]) "second item"
                    , Lazy.lazy (\str -> li [ Attr.class "list-item themed selected" ] [ Html.text str ]) "third item"
                    , Lazy.lazy (\str -> li [ Attr.class "list-item themed" ] [ Html.text str ]) "fourth item"
                    , Lazy.lazy (\str -> span [ Attr.class "tooltip-questions" ] [ Html.text str ]) "?"
                    ]
                ]
            , section []
                [ div [ Attr.class "nested-div" ]
                    [ Html.text "boring section"
                    , Lazy.lazy (\str -> Html.button [ Attr.class "super-button" ] [ Html.text str ]) "click me"
                    , Lazy.lazy (\str -> Html.button [ Attr.class "other-button" ] [ Html.text str ]) "the other button"
                    , Lazy.lazy (\str -> span [ Attr.class "tooltip-questions" ] [ Html.text str ]) "?"
                    ]
                ]
            , footer []
                [ Lazy.lazy2 (\a b -> Html.text <| a ++ b) "this is " "the footer"
                , Lazy.lazy (\str -> span [ Attr.class "tooltip-questions" ] [ Html.text str ]) "?"
                ]
            ]
        ]


someView : String -> Html msg
someView str =
    Html.h1 [] [ Html.text str ]


testHas : Test
testHas =
    describe "Query.has"
        [ fuzz (Fuzz.list Fuzz.string) "Passes for empty selector list" <|
            \strings ->
                Html.div [] (List.map Html.text strings)
                    |> Query.fromHtml
                    |> Query.has []
        ]
