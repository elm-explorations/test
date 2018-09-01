module ShrinkTests exposing (all)

import Expect
import Shrink
import Test exposing (..)


all : Test
all =
    describe "Shrink"
        [ describe "list" <|
            let
                shrink =
                    Shrink.shrink (always True) (Shrink.list Shrink.unit)
            in
            [ test "empty list does not shrink" <|
                \() ->
                    shrink []
                        |> Expect.equal []
            , test "singleton list shrinks to empty" <|
                \() ->
                    shrink [ () ]
                        |> Expect.equal []
            ]
        ]
