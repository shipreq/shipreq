
// currentStyle keeps the info about whether a prop is configured as a spring
// or if it's just a random prop that happens to be present on the style

'use strict';

exports.__esModule = true;
exports['default'] = noVelocity;

function noVelocity(currentVelocity, currentStyle) {
  for (var key in currentVelocity) {
    if (!currentVelocity.hasOwnProperty(key)) {
      continue;
    }
    if (currentStyle[key] != null && currentStyle[key].config && currentVelocity[key] !== 0) {
      return false;
    }
  }
  return true;
}

module.exports = exports['default'];