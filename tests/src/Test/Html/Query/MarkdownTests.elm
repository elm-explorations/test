module Test.Html.Query.MarkdownTests (all) where

import Markdown as Markdown
import Test (..)
import Test as Test
import Test.Html.Query as Query

import Test.Html.Selector (..)
import Test.Html.Selector as Test.Html.Selector


all :: Test
all =
    describe "querying Html that contains markdown nodes from elm-explorations/markdown"
        [ test "can process Html containing an elm-explorations/markdown node" <|
            \{} ->
                Markdown.toHtml List.nil """

# Apple Pie Recipe

  1. Invent the universe.
  2. Bake an apple pie.

"""
                    |> Query.fromHtml
                    |> Query.has
                        [ text "Apple Pie Recipe" ]
        ]
