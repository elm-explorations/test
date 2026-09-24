module Test.Html.Query.CustomNodeTests exposing (all)

import Html exposing (Html, div)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)
import WebGL exposing (Shader)


all : Test
all =
    describe "querying Html that contains other kinds of custom virtual-dom nodes"
        [ test "can process Html containing a WebGL node" <|
            \() ->
                div []
                    [ webGlView
                    , Html.text "hello with webgl"
                    ]
                    |> Query.fromHtml
                    |> Query.has
                        [ text "hello with webgl" ]
        , test "a WebGL canvas matches `tag \"canvas\"` (https://github.com/elm-explorations/test/issues/210)" <|
            \() ->
                webGlView
                    |> Query.fromHtml
                    |> Query.has [ tag "canvas" ]
        , test "a plain div does not match `tag \"canvas\"`" <|
            \() ->
                div [] []
                    |> Query.fromHtml
                    |> Query.hasNot [ tag "canvas" ]
        , test "an empty WebGL canvas (no entities) still matches `tag \"canvas\"`, thanks to its kernel function names" <|
            \() ->
                emptyWebGlView
                    |> Query.fromHtml
                    |> Query.has [ tag "canvas" ]
        , test "a fully-empty WebGL canvas (no entities, no options either) still matches `tag \"canvas\"`" <|
            \() ->
                WebGL.toHtmlWith [] [] []
                    |> Query.fromHtml
                    |> Query.has [ tag "canvas" ]
        ]


webGlView : Html msg
webGlView =
    WebGL.toHtml
        []
        [ WebGL.entity
            vertexShader
            fragmentShader
            (WebGL.triangles [])
            {}
        ]


emptyWebGlView : Html msg
emptyWebGlView =
    WebGL.toHtmlWith
        [ WebGL.alpha True, WebGL.antialias ]
        []
        []


vertexShader : Shader {} {} {}
vertexShader =
    [glsl|
        void main () {
        }
    |]


fragmentShader : Shader {} {} {}
fragmentShader =
    [glsl|
        void main () {
        }
    |]
