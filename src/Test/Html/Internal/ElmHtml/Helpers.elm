module Test.Html.Internal.ElmHtml.Helpers (filterKnownKeys) where

{-| Internal helpers for ElmHtml

@docs filterKnownKeys

-}

import Dict (Dict)
import Dict as Dict
import Test.Html.Internal.ElmHtml.Constants (knownKeys)
import Test.Html.Internal.ElmHtml.Constants as Test.Html.Internal.ElmHtml.Constants


{-| Filter out keys that we don't know
-}
filterKnownKeys :: Dict String a -> Dict String a
filterKnownKeys =
    Dict.filter (\key _ -> not (List.member key knownKeys))
