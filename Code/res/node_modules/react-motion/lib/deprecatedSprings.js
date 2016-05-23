'use strict';

exports.__esModule = true;
exports['default'] = deprecatedSprings;
var hasWarnedForSpring = {};
var hasWarnedForTransitionSpring = {};

function deprecatedSprings(React) {
  var Spring = React.createClass({
    displayName: 'Spring',

    componentWillMount: function componentWillMount() {
      if (process.env.NODE_ENV === 'development') {
        var ownerName = this._reactInternalInstance._currentElement._owner && this._reactInternalInstance._currentElement._owner.getName();
        if (!hasWarnedForSpring[ownerName]) {
          hasWarnedForSpring[ownerName] = true;
          console.error('Spring (used in %srender) has now been renamed to Motion. ' + 'Please see the release note for the upgrade path. Thank you!', ownerName ? ownerName + '\'s ' : 'React.');
        }
      }
    },

    render: function render() {
      return null;
    }
  });

  var TransitionSpring = React.createClass({
    displayName: 'TransitionSpring',

    componentWillMount: function componentWillMount() {
      if (process.env.NODE_ENV === 'development') {
        var ownerName = this._reactInternalInstance._currentElement._owner && this._reactInternalInstance._currentElement._owner.getName();
        if (!hasWarnedForTransitionSpring[ownerName]) {
          hasWarnedForTransitionSpring[ownerName] = true;
          console.error('TransitionSpring (used in %srender) has now been renamed to ' + 'TransitionMotion. Please see the release note for the upgrade ' + 'path. Thank you!', ownerName ? ownerName + '\'s ' : 'React.');
        }
      }
    },

    render: function render() {
      return null;
    }
  });

  return { Spring: Spring, TransitionSpring: TransitionSpring };
}

module.exports = exports['default'];