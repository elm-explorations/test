module Test.Html.ExampleApp {a:exampleModel, b:view} where

import Html (..)
import Html as Html
import Html.Attributes (..)
import Html.Attributes as Html.Attributes
import Html.Events (onClick)
import Html.Events as Html.Events
import Html.Keyed as Keyed

import Html.Lazy as Lazy



type Model =
    {}


exampleModel :: Model
exampleModel =
    {}


data Msg
    = GoToHome
    | GoToExamples


view :: Model -> Html Msg
view _ =
    div [ class "container" ]
        [ header [ class "funky themed", id "heading" ]
            [ a [ href "http://elm-lang.org", onClick GoToHome ] [ text "home" ]
            , a [ href "http://elm-lang.org/examples", onClick GoToExamples ] [ text "examples" ]
            , a [ href "http://elm-lang.org/docs" ] [ text "docs" ]
            ]
        , section [ class "funky themed", id "section" ]
            [ someList ]
        , footer List.nil [ text "this is the footer" ]
        ]


someList :: Html Msg
someList =
    Keyed.ul [ class "some-list" ]
        [ ( "1"
          , Lazy.lazy (\_ -> li [ class "list-item themed" ] [ text "first item" ])
                Nothing
          )
        , ( "2"
          , Lazy.lazy (\_ -> li [ class "list-item themed" ] [ text "second item" ])
                Nothing
          )
        , ( "3"
          , Lazy.lazy (\_ -> li [ class "list-item themed selected" ] [ text "third item" ])
                Nothing
          )
        , ( "4"
          , Lazy.lazy (\_ -> li [ class "list-item themed" ] [ text "fourth item" ])
                Nothing
          )
        ]
