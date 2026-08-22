module BrowserTests exposing (all)

import Browser.Navigation
import Expect
import Fuzz
import Test exposing (Test, describe)


all : Test
all =
    Test.withNavigationKey <|
        \navKey ->
            Test.describe "Test.withNavigationKey"
                [ regularTest navKey
                , fuzzTest navKey
                ]


regularTest : Browser.Navigation.Key -> Test
regularTest navKey =
    Test.test "(regular test) storing, using or comparing the key should not have any noticeable effect" <|
        \() ->
            let
                init : { navKey : Browser.Navigation.Key }
                init =
                    { navKey = navKey }
            in
            ( init, cmdsUsingKey navKey )
                |> Tuple.first
                |> Expect.equal { navKey = navKey }


fuzzTest : Browser.Navigation.Key -> Test
fuzzTest navKey =
    Test.fuzz Fuzz.int "(fuzz) storing, using or comparing the key should not have any noticeable effect" <|
        \number ->
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
