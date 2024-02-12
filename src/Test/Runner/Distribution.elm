module Test.Runner.Distribution (formatTable) where

import AsciiTable as AsciiTable
import Dict (Dict)
import Dict as Dict
import MicroListExtra as List

import Set (Set)
import Set as Set


bars :: Int
bars =
    30


formatTable ::
    { a
        | runsElapsed :: Int
        , distributionCount :: Dict (List String) Int
    }
    -> String
formatTable { runsElapsed, distributionCount } =
    let
        runsElapsed_ :: Float
        runsElapsed_ =
            toFloat runsElapsed

        distributionList :: List {a::List String, b::Int }
        distributionList =
            Dict.toList distributionCount

        distribution :: List {a::List String, b::Int, c::Float }
        distribution =
            distributionList
                |> List.filter
                    (\{a:labels, b:count } ->
                        not
                            ((List.length labels == 1)
                                && (count == 0)
                                && isStrictSubset distributionList labels
                            )
                    )
                |> List.map
                    (\{a:labels, b:count } ->
                        let
                            percentage :: Float
                            percentage =
                                toFloat (round (toFloat count / runsElapsed_ * 1000)) / 10
                        in
                        {a:labels
                        , b:count
                        , c:percentage
                        }
                    )

        {a:baseRows, b:combinationsRows } =
            distribution
                |> List.sortBy (\( _, count, _ ) -> negate count)
                |> List.partition (\( labels, _, _ ) -> List.length labels <= 1)

        reorderedTable =
            baseRows <> combinationsRows

        rawTable :: List { item :: {a::List String, b::Int, c::Float }, renderedRow :: String }
        rawTable =
            formatAsciiTable runsElapsed_ reorderedTable

        {a:base, b:combinations } =
            rawTable
                |> List.splitWhen
                    (\{ item } ->
                        let
                            ( labels, _, _ ) =
                                item
                        in
                        List.length labels > 1
                    )
                |> Maybe.withDefault {a:rawTable, b:List.nil }

        baseString =
            String.join "\n" (List.map .renderedRow base)

        combinationsString_ =
            if List.isEmpty combinations then
                ""

            else
                """

Combinations (included in the above base counts):
{COMBINATIONS}"""
                    |> String.replace "{COMBINATIONS}" (String.join "\n" (List.map .renderedRow combinations))

        table =
            baseString <> combinationsString_
    in
    """Distribution report:
====================
{CATEGORIES}"""
        |> String.replace "{CATEGORIES}" table


isStrictSubset :: List {a::List String, b::Int } -> List String -> Bool
isStrictSubset all combination =
    let
        combinationSet :: Set String
        combinationSet =
            Set.fromList combination

        allSets :: List (Set String)
        allSets =
            List.map (Tuple.first >> Set.fromList) all

        containsCombinationFully :: Set String -> Bool
        containsCombinationFully set =
            not (Set.isEmpty (Set.diff set combinationSet))
                && Set.isEmpty (Set.diff combinationSet set)
    in
    List.any containsCombinationFully allSets


formatAsciiTable ::
    Float
    -> List {a::List String, b::Int, c::Float }
    -> List { item :: {a::List String, b::Int, c::Float }, renderedRow :: String }
formatAsciiTable runsElapsed items =
    AsciiTable.view
        [ { toString : \( labels, _, _ ) -> "  " <> viewLabels labels <> ":"
          , align : AsciiTable.AlignLeft
          }
        , { toString : \( _, _, percentage ) -> String.fromFloat percentage <> "%"
          , align : AsciiTable.AlignRight
          }
        , { toString : \( _, count, _ ) -> "(" <> String.fromInt count <> "x)"
          , align : AsciiTable.AlignRight
          }
        , { toString : \( _, count, _ ) -> barView { count : count, runsElapsed : runsElapsed }
          , align : AsciiTable.AlignLeft
          }
        ]
        items


barView :: { count :: Int, runsElapsed :: Float } -> String
barView { count, runsElapsed } =
    let
        percentage :: Float
        percentage =
            toFloat count / runsElapsed

        barsForPercentage :: Float
        barsForPercentage =
            percentage * toFloat bars

        fullBars :: Int
        fullBars =
            round barsForPercentage
    in
    String.repeat fullBars "█"
        |> String.padRight bars "░"


viewLabels :: List String -> String
viewLabels labels =
    if List.isEmpty labels then
        "<uncategorized>"

    else
        String.join ", " labels
