module DebugConfig exposing
    ( shouldLogFirstFailure
    , shouldLogFuzzTests
    , shouldLogShrinkAttempts
    , shouldLogShrinkProgress
    )


shouldLogFirstFailure : Bool
shouldLogFirstFailure =
    False


shouldLogShrinkProgress : Bool
shouldLogShrinkProgress =
    False


shouldLogShrinkAttempts : Bool
shouldLogShrinkAttempts =
    False


shouldLogFuzzTests : Bool
shouldLogFuzzTests =
    False
