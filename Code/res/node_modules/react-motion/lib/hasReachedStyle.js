'use strict';

exports.__esModule = true;
exports['default'] = hasReachedStyle;

function hasReachedStyle(currentStyle, style) {
  for (var key in style) {
    if (!style.hasOwnProperty(key)) {
      continue;
    }
    var currentValue = currentStyle[key];
    var destValue = style[key];
    if (destValue == null || !destValue.config) {
      // not a spring config
      continue;
    }
    if (currentValue.config && currentValue.val !== destValue.val) {
      return false;
    }
    if (!currentValue.config && currentValue !== destValue.val) {
      return false;
    }
  }

  return true;
}

module.exports = exports['default'];