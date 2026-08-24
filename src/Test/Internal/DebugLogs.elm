module Test.Internal.DebugLogs exposing (Mode, getDebugLogsBeforeFirstTestRun, modeCollect, modeConsoleLog, modeIgnore, noDebugLogsForPassingFuzzTests, rerunFailureToCollectDebugLogs, runTestWithDurationAndCollectDebugLogs)

import Elm.Kernel.DebugLogs
import Json.Encode
import Task exposing (Task)


type Mode
    = Mode


modeConsoleLog : Mode
modeConsoleLog =
    Elm.Kernel.DebugLogs.modeConsoleLog


modeCollect : Mode
modeCollect =
    Elm.Kernel.DebugLogs.modeCollect


modeIgnore : Mode
modeIgnore =
    Elm.Kernel.DebugLogs.modeIgnore


getDebugLogsBeforeFirstTestRun : Task x String
getDebugLogsBeforeFirstTestRun =
    Elm.Kernel.DebugLogs.getDebugLogsBeforeFirstTestRun


runTestWithDurationAndCollectDebugLogs : Mode -> (() -> a) -> (a -> Float -> String -> Bool -> b) -> Task x b
runTestWithDurationAndCollectDebugLogs =
    Elm.Kernel.DebugLogs.runTestWithDurationAndCollectDebugLogs


rerunFailureToCollectDebugLogs : (() -> a) -> String
rerunFailureToCollectDebugLogs =
    Elm.Kernel.DebugLogs.rerunFailureToCollectDebugLogs


noDebugLogsForPassingFuzzTests : String
noDebugLogsForPassingFuzzTests =
    "For passing fuzz tests, Debug.log is not shown, since showing logs from lots of runs is pretty confusing. Tip: Use Debug.todo to fail a test from anywhere if you want some logs to appear."
