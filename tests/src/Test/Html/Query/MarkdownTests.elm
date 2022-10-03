module Test.Html.Query.MarkdownTests exposing (all)

import Markdown
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (text)


all : Test
all =
    describe "querying Html that contains markdown nodes from elm-explorations/markdown"
        [ test "can process Html containing an elm-explorations/markdown node" <|
            \() ->
                Markdown.toHtml [] """

# Apple Pie Recipe

  1. Invent the universe.
  2. Bake an apple pie.

"""
                    |> Query.fromHtml
                    |> Query.has
                        [ text "Apple Pie Recipe" ]
        ]
