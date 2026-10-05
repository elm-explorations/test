/*

import Elm.Kernel.Debug exposing (toString)
import Elm.Kernel.Json exposing (wrap)
import Elm.Kernel.Scheduler exposing (binding, succeed)
import Elm.Kernel.Utils exposing (Tuple0)

*/

var _DebugLogs_modeConsoleLog = __1_CONSOLE_LOG;
var _DebugLogs_modeCollect = __1_COLLECT;
var _DebugLogs_modeIgnore = __1_IGNORE;

var _DebugLogs_logs = '';
var _DebugLogs_used = false;
var _DebugLogs_mode = globalThis.elmTestPrintDebugLogsBeforeFirstTestToConsole ? __1_CONSOLE_LOG : __1_COLLECT;
// Note: Always reset the mode to __1_CONSOLE_LOG, so that `Debug.log` used to debug a test runner
// has a higher chance of showing up out of the box.

var _DebugLogs_logsBeforeFirstTestRun = undefined;

var _DebugLogs_getDebugLogsBeforeFirstTestRun = __Scheduler_binding(function(callback)
{
  if (_DebugLogs_logsBeforeFirstTestRun === undefined) {
    _DebugLogs_logsBeforeFirstTestRun = _DebugLogs_logs;
    _DebugLogs_logs = '';
    _DebugLogs_used = false;
    _DebugLogs_mode = __1_CONSOLE_LOG;
  }
  callback(__Scheduler_succeed(_DebugLogs_logsBeforeFirstTestRun));
});

var _DebugLogs_runTestWithDurationAndCollectDebugLogs = F3(function(mode, thunk, mapper)
{
  return __Scheduler_binding(function(callback)
  {
    if (_DebugLogs_logsBeforeFirstTestRun === undefined) {
      _DebugLogs_logsBeforeFirstTestRun = _DebugLogs_logs;
    }
    _DebugLogs_logs = '';
    _DebugLogs_used = false;
    _DebugLogs_mode = mode;
    var start = performance.now();
    var value = thunk(__Utils_Tuple0);
    var duration = performance.now() - start;
    _DebugLogs_mode = __1_CONSOLE_LOG;
    callback(__Scheduler_succeed(A4(mapper, value, duration, _DebugLogs_logs, _DebugLogs_used)));
  });
});

function _DebugLogs_rerunFailureToCollectDebugLogs(rerunFailure)
{
  _DebugLogs_logs = '';
  _DebugLogs_used = false;
  _DebugLogs_mode = __1_COLLECT;
  rerunFailure(__Utils_Tuple0);
  _DebugLogs_mode = __1_CONSOLE_LOG;
  return _DebugLogs_logs;
}

var _Debug_log = F2(function(tag, value)
{
  _DebugLogs_used = true;
  switch (_DebugLogs_mode) {
    case __1_CONSOLE_LOG:
      console.error(tag + ': ' + __Debug_toString(value));
      break;
    case __1_COLLECT:
      _DebugLogs_logs += tag + ': ' + __Debug_toString(value) + '\n';
      break;
    default:
      // __1_IGNORE: Do nothing.
  }
  return value;
});
