

// this function is allocation-less thanks to babel, which transforms the tail
// calls into loops
'use strict';

exports.__esModule = true;
exports['default'] = mergeDiff;
function mergeDiffArr(_x, _x2, _x3, _x4, _x5, _x6, _x7) {
  var _again = true;

  _function: while (_again) {
    var arrA = _x,
        arrB = _x2,
        collB = _x3,
        indexA = _x4,
        indexB = _x5,
        onRemove = _x6,
        accum = _x7;
    endA = endB = keyA = keyB = fill = fill = undefined;
    _again = false;

    var endA = indexA === arrA.length;
    var endB = indexB === arrB.length;
    var keyA = arrA[indexA];
    var keyB = arrB[indexB];
    if (endA && endB) {
      // returning null here, otherwise lint complains that we're not expecting
      // a return value in subsequent calls. We know what we're doing.
      return null;
    }

    if (endA) {
      accum[keyB] = collB[keyB];
      _x = arrA;
      _x2 = arrB;
      _x3 = collB;
      _x4 = indexA;
      _x5 = indexB + 1;
      _x6 = onRemove;
      _x7 = accum;
      _again = true;
      continue _function;
    }

    if (endB) {
      var fill = onRemove(keyA);
      if (fill != null) {
        accum[keyA] = fill;
      }
      _x = arrA;
      _x2 = arrB;
      _x3 = collB;
      _x4 = indexA + 1;
      _x5 = indexB;
      _x6 = onRemove;
      _x7 = accum;
      _again = true;
      continue _function;
    }

    if (keyA === keyB) {
      accum[keyA] = collB[keyA];
      _x = arrA;
      _x2 = arrB;
      _x3 = collB;
      _x4 = indexA + 1;
      _x5 = indexB + 1;
      _x6 = onRemove;
      _x7 = accum;
      _again = true;
      continue _function;
    }

    if (!collB.hasOwnProperty(keyA)) {
      var fill = onRemove(keyA);
      if (fill != null) {
        accum[keyA] = fill;
      }
      _x = arrA;
      _x2 = arrB;
      _x3 = collB;
      _x4 = indexA + 1;
      _x5 = indexB;
      _x6 = onRemove;
      _x7 = accum;
      _again = true;
      continue _function;
    }

    _x = arrA;
    _x2 = arrB;
    _x3 = collB;
    _x4 = indexA + 1;
    _x5 = indexB;
    _x6 = onRemove;
    _x7 = accum;
    _again = true;
    continue _function;
  }
}

function mergeDiff(a, b, onRemove) {
  var ret = {};
  // if anyone can make this work without allocating the arrays here, we'll
  // give you a medal
  mergeDiffArr(Object.keys(a), Object.keys(b), b, 0, 0, onRemove, ret);
  return ret;
}

module.exports = exports['default'];