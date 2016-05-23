'use strict';

exports.__esModule = true;
exports['default'] = configAnimation;

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { 'default': obj }; }

var _performanceNow = require('performance-now');

var _performanceNow2 = _interopRequireDefault(_performanceNow);

var _raf = require('raf');

var _raf2 = _interopRequireDefault(_raf);

function configAnimation() {
  var config = arguments.length <= 0 || arguments[0] === undefined ? {} : arguments[0];
  var _config$timeStep = config.timeStep;
  var timeStep = _config$timeStep === undefined ? 1 / 60 * 1000 : _config$timeStep;
  var _config$timeScale = config.timeScale;
  var timeScale = _config$timeScale === undefined ? 1 : _config$timeScale;
  var _config$maxSteps = config.maxSteps;
  var maxSteps = _config$maxSteps === undefined ? 10 : _config$maxSteps;
  var _config$raf = config.raf;
  var raf = _config$raf === undefined ? _raf2['default'] : _config$raf;
  var _config$now = config.now;
  var now = _config$now === undefined ? _performanceNow2['default'] : _config$now;

  var animRunning = [];
  var running = false;
  var prevTime = 0;
  var accumulatedTime = 0;

  function loop() {
    var currentTime = now();
    var frameTime = currentTime - prevTime; // delta

    prevTime = currentTime;
    accumulatedTime += frameTime * timeScale;

    if (accumulatedTime > timeStep * maxSteps) {
      accumulatedTime = 0;
    }

    var frameNumber = Math.ceil(accumulatedTime / timeStep);
    for (var i = 0; i < animRunning.length; i++) {
      var _animRunning$i = animRunning[i];
      var active = _animRunning$i.active;
      var animationStep = _animRunning$i.animationStep;
      var prevPrevState = _animRunning$i.prevState;
      var prevNextState = animRunning[i].nextState;

      if (!active) {
        continue;
      }

      // Seems like because the TS sets destVals as enterVals for the first
      // tick, we might render that value twice. We render it once, currValue is
      // enterVal and destVal is enterVal. The next tick is faster than 16ms,
      // so accumulatedTime (which would be about -16ms from the previous tick)
      // is negative (-16ms + any number less than 16ms < 0). So we just render
      // part ways towards the nextState, but that's enterVal still. We render
      // say 75% between currValue (=== enterVal) and destValue (=== enterVal).
      // So we render the same value a second time.
      // The solution below is to recalculate the destination state even when
      // you're moving partially towards it.
      if (accumulatedTime <= 0) {
        animRunning[i].nextState = animationStep(timeStep / 1000, prevPrevState);
      } else {
        for (var j = 0; j < frameNumber; j++) {
          animRunning[i].nextState = animationStep(timeStep / 1000, prevNextState);
          var _ref = [prevNextState, animRunning[i].nextState];
          animRunning[i].prevState = _ref[0];
          prevNextState = _ref[1];
        }
      }
    }

    accumulatedTime = accumulatedTime - frameNumber * timeStep;

    // Render and filter in one iteration.
    var alpha = 1 + accumulatedTime / timeStep;
    for (var i = 0; i < animRunning.length; i++) {
      var _animRunning$i2 = animRunning[i];
      var animationRender = _animRunning$i2.animationRender;
      var nextState = _animRunning$i2.nextState;
      var prevState = _animRunning$i2.prevState;

      // Might mutate animRunning........
      animationRender(alpha, nextState, prevState);
    }

    animRunning = animRunning.filter(function (_ref2) {
      var active = _ref2.active;
      return active;
    });

    if (animRunning.length === 0) {
      running = false;
    } else {
      raf(loop);
    }
  }

  function start() {
    if (!running) {
      running = true;
      prevTime = now();
      accumulatedTime = 0;
      raf(loop);
    }
  }

  return function startAnimation(state, animationStep, animationRender) {
    for (var i = 0; i < animRunning.length; i++) {
      var val = animRunning[i];
      if (val.animationStep === animationStep) {
        val.active = true;
        val.prevState = state;
        start();
        return val.stop;
      }
    }

    var newAnim = {
      animationStep: animationStep,
      animationRender: animationRender,
      prevState: state,
      nextState: state,
      active: true
    };

    newAnim.stop = function () {
      return newAnim.active = false;
    };
    animRunning.push(newAnim);

    start();

    return newAnim.stop;
  };
}

module.exports = exports['default'];