module Test.Html.Query.CustomNodeTests exposing (all)

import Html exposing (Html, div)
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text)
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
