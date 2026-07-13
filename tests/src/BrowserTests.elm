module BrowserTests exposing (all)

import Browser.Navigation
import Expect
import Fuzz
import Test exposing (Test, describe)


all : Test
all =
    Test.concat
        [ testWithKey
        , fuzzWithKey
        ]


testWithKey : Test
testWithKey =
    Test.testWithKey "Test.testWithKey: storing, using or comparing the key should not have any noticeable effect" <|
        \navKey ->
            let
                init : { navKey : Browser.Navigation.Key }
                init =
                    { navKey = navKey }
            in
            ( init, cmdsUsingKey navKey )
                |> Tuple.first
                |> Expect.equal { navKey = navKey }


fuzzWithKey : Test
fuzzWithKey =
    Test.fuzzWithKey Fuzz.int "Test.fuzzWithKey: storing, using or comparing the key should not have any noticeable effect" <|
        \navKey number ->
            let
                init : { navKey : Browser.Navigation.Key, number : Int }
                init =
                    { navKey = navKey, number = number }
            in
            ( init, cmdsUsingKey navKey )
                |> Tuple.first
                |> Expect.equal { navKey = navKey, number = number }


cmdsUsingKey : Browser.Navigation.Key -> Cmd msg
cmdsUsingKey navKey =
    Cmd.batch
        [ Browser.Navigation.pushUrl navKey "some-url"
        , Browser.Navigation.replaceUrl navKey "some-url"
        , Browser.Navigation.back navKey 1
        , Browser.Navigation.forward navKey 1
        ]
