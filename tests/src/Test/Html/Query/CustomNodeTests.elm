module Test.Html.Query.CustomNodeTests exposing (all)

import Expect
import Html exposing (Html, div)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)
import WebGL exposing (Mesh, Shader)


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
