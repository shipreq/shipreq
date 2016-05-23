"use strict";

exports.__esModule = true;
exports["default"] = reorderKeys;

function reorderKeys(obj, f) {
  var newKeys = f(Object.keys(obj));
  var ret = {};
  for (var i = 0; i < newKeys.length; i++) {
    var key = newKeys[i];
    ret[key] = obj[key];
  }

  return ret;
}

module.exports = exports["default"];