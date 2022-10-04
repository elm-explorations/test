module Runner.String.Format exposing (format)

import Diff exposing (Change(..))
import Test.Runner.Failure exposing (InvalidReason(..), Reason(..))


format : String -> Reason -> String
format description reason =
    case reason of
        Custom ->
            description

        Equality expected actual ->
            equalityToString { operation = description, expected = expected, actual = actual }

        Comparison first second ->
            verticalBar description first second

        TODO ->
            description

        Invalid BadDescription ->
            if description == "" then
                "The empty string is not a valid test description."

            else
                "This is an invalid test description: " ++ description

        Invalid _ ->
            description

        ListDiff expected actual ->
            listDiffToString 0
                description
                { expected = expected
                , actual = actual
                }
                { originalExpected = expected
                , originalActual = actual
                }

        CollectionDiff { expected, actual, extra, missing } ->
            let
                extraStr =
                    if List.isEmpty extra then
                        ""

                    else
                        "\nThese keys are extra: "
                            ++ (extra |> String.join ", " |> (\d -> "[ " ++ d ++ " ]"))

                missingStr =
                    if List.isEmpty missing then
                        ""

                    else
                        "\nThese keys are missing: "
                            ++ (missing |> String.join ", " |> (\d -> "[ " ++ d ++ " ]"))
            in
            String.concat
                [ verticalBar description expected actual
                , "\n"
                , extraStr
                , missingStr
                ]


verticalBar : String -> String -> String -> String
verticalBar comparison below above =
    [ above
    , "╵"
    , "│ |> " ++ comparison
    , "╷"
    , below
    ]
        |> String.join "\n"


hexInt : Int -> String
hexInt int =
    if int == 0 then
        "0"

    else
        let
            zeroPad4 n =
                if String.length n < 4 then
                    zeroPad4 ("0" ++ n)

                else
                    n

            hexIntInternal i =
                if i == 0 then
                    ""

                else
                    hexIntInternal (i // 16)
                        ++ (case i |> remainderBy 16 of
                                10 ->
                                    "a"

                                11 ->
                                    "b"

                                12 ->
                                    "c"

                                13 ->
                                    "d"

                                14 ->
                                    "e"

                                15 ->
                                    "f"

                                decimalDigit ->
                                    String.fromInt decimalDigit
                           )
        in
        zeroPad4 (hexIntInternal int)


escapeUnicodeChars : String -> String
escapeUnicodeChars s =
    let
        isAsciiChar v =
            (32 <= v && v <= 125)
                -- in ascii a-z A-Z with a few common special characters
                && (-- except ` and ^ because they're combining marks, like in è and ê.
                    List.member v [ 94, 96 ] == False
                   )
    in
    s
        |> String.toList
        |> List.map Char.toCode
        |> List.map
            (\c ->
                if isAsciiChar c then
                    String.fromChar (Char.fromCode c)

                else
                    "\\u{" ++ hexInt c ++ "}"
            )
        |> String.concat


listDiffToString :
    Int
    -> String
    -> { expected : List String, actual : List String }
    -> { originalExpected : List String, originalActual : List String }
    -> String
listDiffToString index description { expected, actual } originals =
    case ( expected, actual ) of
        ( [], [] ) ->
            [ "Two lists were unequal previously, yet ended up equal later."
            , "This should never happen!"
            , "Please report this bug to https://github.com/elm-explorations/test/issues - and include these lists: "
            , "\n"
            , Debug.toString originals.originalExpected
            , "\n"
            , Debug.toString originals.originalActual
            ]
                |> String.concat

        ( _ :: _, [] ) ->
            verticalBar (description ++ " was shorter than")
                (Debug.toString originals.originalExpected)
                (Debug.toString originals.originalActual)

        ( [], _ :: _ ) ->
            verticalBar (description ++ " was longer than")
                (Debug.toString originals.originalExpected)
                (Debug.toString originals.originalActual)

        ( firstExpected :: restExpected, firstActual :: restActual ) ->
            if firstExpected == firstActual then
                -- They're still the same so far; keep going.
                listDiffToString (index + 1)
                    description
                    { expected = restExpected
                    , actual = restActual
                    }
                    originals

            else
                -- We found elements that differ; fail!
                String.concat
                    [ verticalBar description
                        (Debug.toString originals.originalExpected)
                        (Debug.toString originals.originalActual)
                    , "\n\nThe first diff is at index "
                    , Debug.toString index
                    , ": it was `"
                    , firstActual
                    , "`, but `"
                    , firstExpected
                    , "` was expected."
                    ]


equalityToString : { operation : String, expected : String, actual : String } -> String
equalityToString { operation, expected, actual } =
    -- TODO make sure this looks reasonable for multiline strings
    let
        ( ( valueBelow, diffArrowsBelow ), ( diffArrowsAbove, valueAbove ) ) =
            formatEqualityDiffArrows expected actual

        ( ( unicodeValueBelow, unicodeDiffArrowsBelow ), ( unicodeDiffArrowsAbove, unicodeValueAbove ) ) =
            formatEqualityDiffArrows (escapeUnicodeChars expected) (escapeUnicodeChars actual)

        combine things =
            things
                |> List.map String.concat
                |> String.join "\n"
    in
    verticalBar
        operation
        (if String.concat valueBelow /= String.concat unicodeValueBelow then
            -- we need to show the escaped string as well
            combine
                [ valueBelow
                , diffArrowsBelow
                , unicodeValueBelow ++ [ " (same string but with unicode characters escaped)" ]
                , unicodeDiffArrowsBelow
                ]

         else
            combine
                [ valueBelow
                , diffArrowsBelow
                ]
        )
        (if String.concat valueAbove /= String.concat unicodeValueAbove then
            -- we need to show the escaped string as well
            combine
                [ unicodeDiffArrowsAbove
                , unicodeValueAbove ++ [ " (same string but with unicode characters escaped)" ]
                , diffArrowsAbove
                , valueAbove
                ]

         else
            combine
                [ diffArrowsAbove
                , valueAbove
                ]
        )


formatEqualityDiffArrows : String -> String -> ( ( List String, List String ), ( List String, List String ) )
formatEqualityDiffArrows below above =
    if String.length below * String.length above > 300 * 300 then
        -- The Diff.diff diffing algorithm is roughly O(len(a) * len(b)), so we need some cutoff point where we just give up on diffing.
        ( ( [ below ], [ " -- skipping diffing because input is too large" ] ), ( [ " -- skipping diffing because input is too large" ], [ above ] ) )

    else
        let
            ( valueBelow, diffArrowsBelow ) =
                Diff.diff (String.toList below) (String.toList above)
                    |> List.map formatExpectedChange
                    |> List.unzip

            ( diffArrowsAbove, valueAbove ) =
                Diff.diff (String.toList above) (String.toList below)
                    |> List.map formatActualChange
                    |> List.unzip
        in
        ( ( valueBelow, diffArrowsBelow ), ( diffArrowsAbove, valueAbove ) )


formatExpectedChange : Change Char -> ( String, String )
formatExpectedChange diff =
    case diff of
        Added _ ->
            ( "", "" )

        Removed char ->
            ( String.fromChar char, "▲" )

        NoChange char ->
            ( String.fromChar char, " " )


formatActualChange : Change Char -> ( String, String )
formatActualChange diff =
    case diff of
        Added _ ->
            ( "", "" )

        Removed char ->
            ( "▼", String.fromChar char )

        NoChange char ->
            ( " ", String.fromChar char )
