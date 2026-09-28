module Test.Runner.Failure exposing (Reason(..), InvalidReason(..), flatten)

{-| The reason a test failed.

@docs Reason, InvalidReason, flatten

-}


{-| The reason a test failed.

Test runners can use this to provide nice output, e.g. by doing diffs on the
two parts of an `Expect.equal` failure.

-}
type Reason
    = Custom
    | Equality String String
    | Comparison String String
      -- Expected, actual, (index of problem, expected element, actual element)
    | ListDiff (List String) (List String)
      {- I don't think we need to show the diff twice with + and - reversed. Just show it after the main vertical bar.
         "Extra" and "missing" are relative to the actual value.
      -}
    | CollectionDiff
        { expected : String
        , actual : String
        , extra : List String
        , missing : List String
        }
    | TODO
    | Invalid InvalidReason
      -- The failures of each of several expectations that were all tried and
      -- all failed, e.g. via `Expect.oneOf`.
    | Multiple
        (List
            { given : Maybe String
            , description : String
            , reason : Reason
            }
        )


{-| The reason a test run was invalid.

Test runners should report these to the user in whatever format is appropriate.

-}
type InvalidReason
    = EmptyList
    | NonpositiveFuzzCount
    | InvalidFuzzer
    | BadDescription
    | DuplicatedName
    | DistributionInsufficient
    | DistributionBug


{-| Flatten nested `Multiple` reasons into a single level.

Gets rid of the need for nested indentation when reporting nested `Expect.oneOf`
failures.

-}
flatten : Reason -> Reason
flatten reason =
    case reason of
        Multiple failures ->
            Multiple (flattenList failures)

        Custom ->
            reason

        Equality _ _ ->
            reason

        Comparison _ _ ->
            reason

        ListDiff _ _ ->
            reason

        CollectionDiff _ ->
            reason

        TODO ->
            reason

        Invalid _ ->
            reason


flattenList :
    List { given : Maybe String, description : String, reason : Reason }
    -> List { given : Maybe String, description : String, reason : Reason }
flattenList failures =
    List.concatMap
        (\failure ->
            case failure.reason of
                Multiple inner ->
                    flattenList inner

                Custom ->
                    [ failure ]

                Equality _ _ ->
                    [ failure ]

                Comparison _ _ ->
                    [ failure ]

                ListDiff _ _ ->
                    [ failure ]

                CollectionDiff _ ->
                    [ failure ]

                TODO ->
                    [ failure ]

                Invalid _ ->
                    [ failure ]
        )
        failures
