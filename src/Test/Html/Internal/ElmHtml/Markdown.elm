module Test.Html.Internal.ElmHtml.Markdown ( MarkdownOptions, MarkdownModel, baseMarkdownModel
    , decodeMarkdownModel
    )
 where

{-| Markdown helpers

@docs MarkdownOptions, MarkdownModel, baseMarkdownModel

@docs decodeMarkdownModel

-}

import Json.Decode (field)
import Json.Decode as Json.Decode
import Test.Internal.KernelConstants (kernelConstants)
import Test.Internal.KernelConstants as Test.Internal.KernelConstants


{-| Just a default markdown model
-}
baseMarkdownModel :: MarkdownModel
baseMarkdownModel =
    { options :
        { githubFlavored : Just { tables : False, breaks : False }
        , defaultHighlighting : Nothing
        , sanitize : False
        , smartypants : False
        }
    , markdown : ""
    }


{-| options markdown expects
-}
type MarkdownOptions =
    { githubFlavored :: Maybe { tables :: Bool, breaks :: Bool }
    , defaultHighlighting :: Maybe String
    , sanitize :: Bool
    , smartypants :: Bool
    }


{-| An internal markdown model. Options are the things you give markdown, markdown is the string
-}
type MarkdownModel =
    { options :: MarkdownOptions
    , markdown :: String
    }


{-| decode a markdown model
-}
decodeMarkdownModel :: Json.Decode.Decoder MarkdownModel
decodeMarkdownModel =
    field kernelConstants.markdown.markdown Json.Decode.string
        |> Json.Decode.map (MarkdownModel baseMarkdownModel.options)
