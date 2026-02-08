// Examples of how 3rd party coverage tooling could patch the dummy functions - START
// TODO remove these
var _EdgeCoverage_previousSeenHits;
var _EdgeCoverage_currentSeenHits;
var _EdgeCoverage_edgeHitCounts;
var _EdgeCoverage_currentPointId;
var _EdgeCoverage_previousPointId;
var _EdgeCoverage_currentEdgeId;
var _EdgeCoverage_testStartTime;

const _EdgeCoverage_bucketTable = new Uint8Array(256);
for (let i = 0; i < 256; i++) {
  _EdgeCoverage_bucketTable[i] = (i <   4) ? i : 
                                 (i <   8) ? 4 :
                                 (i <  16) ? 5 :
                                 (i <  32) ? 6 :
                                 (i < 128) ? 7 : 8;
}

function resetEdgeCoverage(_) {
  _EdgeCoverage_previousSeenHits = new Set();
  _EdgeCoverage_currentSeenHits = new Set();
  _EdgeCoverage_edgeHitCounts = new Uint8ClampedArray(65536);
  _EdgeCoverage_currentPointId = 0;
  _EdgeCoverage_previousPointId = 0;
  _EdgeCoverage_currentEdgeId = 0;
  _EdgeCoverage_testStartTime = performance.now();
}

function track(pointId) {
  _EdgeCoverage_currentPoint = pointId;
  _EdgeCoverage_currentEdgeId = (_EdgeCoverage_currentPointId ^ _EdgeCoverage_previousPointId) % 65536;
  _EdgeCoverage_previousPointId = _EdgeCoverage_currentPointId >> 1;

  _EdgeCoverage_edgeHitCounts[_EdgeCoverage_currentEdgeId]++;
  _EdgeCoverage_currentSeenHits.add(_EdgeCoverage_currentEdgeId);
}

function getEdgeCoverage(_) {
  var newPaths = _EdgeCoverage_currentSeenHits.size - _EdgeCoverage_previousSeenHits.size;
  _EdgeCoverage_previousSeenHits = _EdgeCoverage_currentSeenHits;

  var durationMs = performance.now() - _EdgeCoverage_testStartTime;

  return {
    // TODO What more?
    edgeHitCounts: _EdgeCoverage_edgeHitCounts, // Does Elm just hold this for the corpus?
    durationMs: durationMs,
    newPaths: newPaths,
  };
}
// Examples of how 3rd party coverage tooling could patch the dummy functions - END




// TODO remove these and use something useful
var _EdgeCoverage_bucketEdgeHitCounts = function(edgeHitCounts) {
  // TODO
  return 0;
}
