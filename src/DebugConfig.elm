module DebugConfig exposing
    ( shouldLogFirstFailure
    , shouldLogFuzzTests
    , shouldLogSimplifyAttempts
    , shouldLogSimplifyProgress
    )


shouldLogFirstFailure : Bool
shouldLogFirstFailure =
    False


shouldLogSimplifyProgress : Bool
shouldLogSimplifyProgress =
    False


shouldLogSimplifyAttempts : Bool
shouldLogSimplifyAttempts =
    False


shouldLogFuzzTests : Bool
shouldLogFuzzTests =
    False
