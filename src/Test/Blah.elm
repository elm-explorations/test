module Test.Blah exposing (now)

import Expect exposing (Expectation)
import Task exposing (Task)


now :
    (Float -> Maybe (Task x2 a2) -> Expectation)
    -> Task x1 a1
    -> Expectation
now =
    Debug.todo "Blah"
