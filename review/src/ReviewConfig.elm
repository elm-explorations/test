module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Docs.NoMissing exposing (exposedModules, onlyExposed)
import Docs.ReviewAtDocs
import Docs.ReviewLinksAndSections
import Docs.UpToDateReadmeLinks
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoImportingEverything
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeExpose
import NoPrematureLetComputation
import NoSimpleLetBody
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule as Rule exposing (Rule)
import Simplify


config : List Rule
config =
    [ NoExposingEverything.rule
    , NoImportingEverything.rule []
        |> Rule.ignoreErrorsForFiles
            [ "src/Test/Html/Internal/ElmHtml/ToString.elm"
            , "src/Test/Html/Internal/ElmHtml/Query.elm"
            ]
    , NoMissingTypeAnnotation.rule
        |> Rule.ignoreErrorsForFiles [ "src/Test/Internal/KernelConstants.elm" ]
    , NoMissingTypeExpose.rule
    , NoSimpleLetBody.rule
        |> Rule.ignoreErrorsForFiles [ "src/Simplify.elm" ]
    , NoPrematureLetComputation.rule
    , NoUnused.CustomTypeConstructors.rule []
        |> Rule.ignoreErrorsForFiles [ "src/Test/Html/Internal/ElmHtml/InternalTypes.elm" ]
    , NoUnused.CustomTypeConstructorArgs.rule
        |> Rule.ignoreErrorsForFiles [ "src/Test/Html/Internal/ElmHtml/InternalTypes.elm" ]
    , NoUnused.Exports.rule
        |> Rule.ignoreErrorsForFiles
            [ "src/Test/Html/Internal/ElmHtml/ToString.elm"
            , "src/Test/Html/Internal/ElmHtml/Query.elm"
            , "src/Test/Html/Internal/ElmHtml/InternalTypes.elm"
            ]
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
        |> Rule.ignoreErrorsForFiles [ "src/Simplify.elm" ]
    , Simplify.rule Simplify.defaults
        |> Rule.ignoreErrorsForFiles [ "src/Fuzz.elm" ]
    ]
