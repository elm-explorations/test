module Expect.Effect exposing
    ( Effect
    , EffectExpectation
    , Key
    , completed
    , failed
    , fromCmd
    , fromTask
    , httpRequestBytes
    , httpRequestString
    , timeNow
    , toExpectation
    )

import Bytes exposing (Bytes)
import Expect exposing (Expectation)
import Http
import Task exposing (Task)
import Time


{-| Put this type in `Test.Effect` so it can be created by `Test.Effect.test` etc.
-}
type Key
    = Key


type Effect x a
    = Effect


fromCmd : Key -> Cmd msg -> Effect never msg
fromCmd key cmd =
    Debug.todo "Turn a Cmd into an Effect using Kernel code"


fromTask : Key -> Task x a -> Effect x a
fromTask key task =
    Debug.todo "Turn a Cmd into an Effect using Kernel code"


type EffectExpectation
    = EffectExpectation


toExpectation : EffectExpectation -> Expectation
toExpectation =
    Debug.todo "Kernel code"


httpRequestString :
    ({ url : String, body : String }
     -> (Http.Response String -> Effect x a)
     -> EffectExpectation
    )
    -> Effect x a
    -> EffectExpectation
httpRequestString =
    Debug.todo "Kernel code"


httpRequestBytes :
    ({ url : String, body : Bytes }
     -> (Http.Response Bytes -> Effect x a)
     -> EffectExpectation
    )
    -> Effect x a
    -> EffectExpectation
httpRequestBytes =
    Debug.todo "Kernel code"


timeNow :
    ((Time.Posix -> Effect x a) -> EffectExpectation)
    -> Effect x a
    -> EffectExpectation
timeNow =
    Debug.todo "Kernel code"


completed : (a -> Expectation) -> Effect x a -> EffectExpectation
completed =
    Debug.todo "Kernel code"


failed : (x -> Expectation) -> Effect x a -> EffectExpectation
failed =
    Debug.todo "Kernel code"
