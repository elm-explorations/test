/*

import Basics exposing (EQ, LT, GT)
import Maybe exposing (Just, Nothing)
import Elm.Kernel.List exposing (Nil, Cons)

*/

// TODO rename Chunk.size to Chunk.length?

var _RandomRun_empty = new Uint32Array();

function _RandomRun_isEmpty(rr) {
  return rr.length === 0;
}

var _RandomRun_compare = F2(function _RandomRun_compare_(a, b) {
  if (a.length < b.length) return __Basics_LT;
  if (a.length > b.length) return __Basics_GT;
  if (a.length === 0) return __Basics_EQ;
  for (var i = 0; i < a.length; i++) {
    var ai = a[i];
    var bi = b[i];
    if (ai < bi) return __Basics_LT;
    if (ai > bi) return __Basics_GT;
  }
  return __Basics_EQ;
});

var _RandomRun_equal = F2(function _RandomRun_equal_(a, b) {
  return A2(_RandomRun_compare, a, b) === __Basics_EQ;
});

function _RandomRun_nextChoice(rr) {
  if (rr.length === 0) {
    return __Maybe_Nothing;
  }
  return __Maybe_Just({
    a: rr[0],
    b: rr.subarray(1), // Rest of the array. We're not copying here (.slice(1)),
                       // we're reusing the original buffer (.subarray(1)).
                       // This wouldn't be safe if writes were mutating, but
                       // they're not (they're always copying).
  });
}

function _RandomRun_length(rr) {
  return rr.length;
}

function _RandomRun_toList(rr) {
  var out = __List_Nil;
  for (var i = rr.length - 1; i >= 0; i--) {
    out = __List_Cons(rr[i], out);
  }
  return out;
}

var _RandomRun_get = F2(function _RandomRun_get_(index, rr) {
  var item = rr[index];
  if (item === undefined) {
    return __Maybe_Nothing;
  }
  return __Maybe_Just(item);
});

var _RandomRun_set = F3(function _RandomRun_set_(index, value, rr) {
  if (index < 0 || index >= rr.length) {
    return rr;
  }
  return rr.with(index, Math.max(0, value));
});

var _RandomRun_replace = F2(function _RandomRun_replace_(values, rr) {
  var out = rr;
  for (; values.b; values = values.b) // WHILE_CONS
  {
    var index = values.a.a;
    if (index >= rr.length) continue; // Ignore indexes out of bounds.
    var value = Math.max(0, values.a.b);
    out = out.with(index, value);
  }
  return out;
});

var _RandomRun_update = F3(function _RandomRun_update_(index, fn, rr) {
  if (index < 0 || index >= rr.length) {
    return rr;
  }
  return rr.with(index, Math.max(0, fn(rr[index])));
});

var _RandomRun_append = F2(function _RandomRun_append_(value, rr) {
  // I think there is a Rust-like world in which we could carefully track
  // whether we have the only handle on the rr, and mutate, but that would
  // bring a whole new level of complexity to using the Elm API of this JS
  // kernel module. Let's not do that.

  var out = new Uint32Array(rr.length + 1);
  out.set(rr);
  out[rr.length] = Math.max(0, value);
  return out;
});

function _RandomRun_isInBounds(chunk, rr) {
  // TODO would precomputing this sum into Chunks be faster?
  return chunk.__$startIndex + chunk.__$size <= rr.length;
};

var _RandomRun_replaceChunkWithZero = F2(function _RandomRun_replaceChunkWithZero_(chunk, rr) {
  if (!_RandomRun_isInBounds(chunk, rr)) {
    return rr;
  }
  var out = new Uint32Array(rr.length);
  out.set(rr);
  var end = chunk.__$startIndex + chunk.__$size;
  for (var i = chunk.__$startIndex; i < end; i++) {
    out[i] = 0;
  }
  return out;
});

var _RandomRun_swapChunks = F2(function _RandomRun_swapChunks_(chunks, rr) {
  if (!_RandomRun_isInBounds(chunks.__$leftChunk, rr)
    || !_RandomRun_isInBounds(chunks.__$rightChunk, rr)) {
    return __Maybe_Nothing;
  }

  // What happens when the two chunks overlap:
  // We first write the left chunk to the right position,
  // then the right chunk to the left position.
  // Note we take the original values, not the intermediate ones.
  //
  // abcdefg.........
  // .....XYZ........
  // =>
  // abcdeabcdefg....
  // =>
  // XYZdeabcdefg.... (not abcdeabcdefg....)

  var out = new Uint32Array(rr.length);
  out.set(rr);

  // TODO extract a helper? might be useful for replace() or replaceChunkWithZero() too?
  // write original rights to left
  var leftEnd = chunks.__$leftChunk.__$startIndex + chunks.__$leftChunk.__$size;
  for (var from = chunks.__$leftChunk.__$startIndex, to = chunks.__$rightChunk.__$startIndex;
    from < leftEnd;
    from++, to++) {
    out[to] = rr[from];
  }

  // write original lefts to right
  var rightEnd = chunks.__$rightChunk.__$startIndex + chunks.__$rightChunk.__$size;
  for (var from = chunks.__$rightChunk.__$startIndex, to = chunks.__$leftChunk.__$startIndex;
    from < rightEnd;
    from++, to++) {
    out[to] = rr[from];
  }

  return __Maybe_Just(out);
});

var _RandomRun_deleteChunk = F2(function _RandomRun_deleteChunk_(chunk, rr) {
  if (!_RandomRun_isInBounds(chunk, rr)) {
    return rr;
  }
  var out = new Uint32Array(rr.length - chunk.__$size);
  out.set(rr.subarray(0, chunk.__$startIndex));
  out.set(rr.subarray(chunk.__$startIndex + chunk.__$size), chunk.__$startIndex);
  return out;
});

var _RandomRun_swapIfOutOfOrder = F2(function _RandomRun_swapIfOutOfOrder_(indices, rr) {
  var left = rr[indices.__$leftIndex];
  if (left === undefined) return __Maybe_Nothing;
  var right = rr[indices.__$rightIndex];
  if (right === undefined) return __Maybe_Nothing;

  if (left > right) { // out of order
    return __Maybe_Just({
      newRun: rr.with(indices.__$leftIndex, right).with(indices.__$rightIndex, left),
      newLeftValue: right,
      newRightValue: left,
    });
  } else { // left <= right, they weren't out of order
    return __Maybe_Just({
      newRun: rr,
      newLeftValue: left,
      newRightValue: right,
    });
  }
});

var _RandomRun_sortChunk = F2(function _RandomRun_sortChunk_(chunk, rr) {
  if (!_RandomRun_isInBounds(chunk, rr)) {
    return rr;
  }
  var out = new Uint32Array(rr.length);
  out.set(rr);
  out.subarray(chunk.__$startIndex, chunk.__$startIndex + chunk.__$size).sort();
  return out;
});
