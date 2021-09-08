module Test.Effect exposing (Key, test)

import Expect.Effect exposing (EffectExpectation)
import Test exposing (Test)


type alias Key =
    Expect.Effect.Key


test : String -> (Key -> EffectExpectation) -> Test
test =
    Debug.todo "Kernel code"
