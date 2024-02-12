module Test.Html.Query.CustomNodeTests (all) where

import Html (Html, div)
import Html as Html
import Test (..)
import Test as Test
import Test.Html.Query as Query

import Test.Html.Selector (..)
import Test.Html.Selector as Test.Html.Selector
import WebGL (Shader)
import WebGL as WebGL


all :: Test
all =
    describe "querying Html that contains other kinds of custom virtual-dom nodes"
        [ test "can process Html containing a WebGL node" <|
            \{} ->
                div List.nil
                    [ webGlView
                    , Html.text "hello with webgl"
                    ]
                    |> Query.fromHtml
                    |> Query.has
                        [ text "hello with webgl" ]
        ]


webGlView :: Html msg
webGlView =
    WebGL.toHtml
        List.nil
        [ WebGL.entity
            vertexShader
            fragmentShader
            (WebGL.triangles List.nil)
            {}
        ]


vertexShader :: Shader {} {} {}
vertexShader =
    [glsl|
        void main {} {
        }
    |]


fragmentShader :: Shader {} {} {}
fragmentShader =
    [glsl|
        void main {} {
        }
    |]
