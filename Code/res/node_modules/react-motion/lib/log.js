/* eslint-disable no-console */

"use strict";

exports.__esModule = true;
exports["default"] = l;

function l() {
  var stuffToLog = arguments.length <= 0 || arguments[0] === undefined ? this : arguments[0];

  console.log(stuffToLog);
  return this;
}

// [1, 2, 3].map(x => x + 1)::l().filter(x => x < 3)::l()
// [2, 3, 4]
// [2]
// wow wat

// <Spring endValue={...}>
//   {() =>
//     <div>
//       bla
//     </div>
//     ::log('hi')::log('keystrokes saver')
//   }
// </Spring>
module.exports = exports["default"];