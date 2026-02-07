module DebugConfig exposing
    ( shouldLogFirstFailure
    , shouldLogFuzzTests
    , shouldLogShrinkAttempts
    , shouldLogShrinkProgress
    )


shouldLogFirstFailure : Bool
shouldLogFirstFailure =
    True


shouldLogShrinkProgress : Bool
shouldLogShrinkProgress =
    True


shouldLogShrinkAttempts : Bool
shouldLogShrinkAttempts =
    True


shouldLogFuzzTests : Bool
shouldLogFuzzTests =
    True
