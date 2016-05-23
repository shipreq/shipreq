'use strict';

exports.__esModule = true;

var _extends = Object.assign || function (target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i]; for (var key in source) { if (Object.prototype.hasOwnProperty.call(source, key)) { target[key] = source[key]; } } } return target; };

exports['default'] = components;

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { 'default': obj }; }

var _noVelocity = require('./noVelocity');

var _noVelocity2 = _interopRequireDefault(_noVelocity);

var _hasReachedStyle = require('./hasReachedStyle');

var _hasReachedStyle2 = _interopRequireDefault(_hasReachedStyle);

var _mergeDiff = require('./mergeDiff');

var _mergeDiff2 = _interopRequireDefault(_mergeDiff);

var _animationLoop = require('./animationLoop');

var _animationLoop2 = _interopRequireDefault(_animationLoop);

var _zero = require('./zero');

var _zero2 = _interopRequireDefault(_zero);

var _updateTree = require('./updateTree');

var _deprecatedSprings2 = require('./deprecatedSprings');

var _deprecatedSprings3 = _interopRequireDefault(_deprecatedSprings2);

var _stripStyle = require('./stripStyle');

var _stripStyle2 = _interopRequireDefault(_stripStyle);

var startAnimation = _animationLoop2['default']();

function mapObject(f, obj) {
  var ret = {};
  for (var key in obj) {
    if (!obj.hasOwnProperty(key)) {
      continue;
    }
    ret[key] = f(obj[key], key);
  }
  return ret;
}

function everyObj(f, obj) {
  for (var key in obj) {
    if (!obj.hasOwnProperty(key)) {
      continue;
    }
    if (!f(obj[key], key)) {
      return false;
    }
  }
  return true;
}

function components(React) {
  var PropTypes = React.PropTypes;

  var Motion = React.createClass({
    displayName: 'Motion',

    propTypes: {
      // TOOD: warn against putting a config in here
      defaultValue: function defaultValue(prop, propName) {
        if (prop[propName]) {
          return new Error('Spring\'s `defaultValue` has been changed to `defaultStyle`. ' + 'Its format received a few (easy to update!) changes as well.');
        }
      },
      endValue: function endValue(prop, propName) {
        if (prop[propName]) {
          return new Error('Spring\'s `endValue` has been changed to `style`. Its format ' + 'received a few (easy to update!) changes as well.');
        }
      },
      defaultStyle: PropTypes.object,
      style: PropTypes.object.isRequired,
      children: PropTypes.func.isRequired
    },

    getInitialState: function getInitialState() {
      var _props = this.props;
      var defaultStyle = _props.defaultStyle;
      var style = _props.style;

      var currentStyle = defaultStyle || style;
      return {
        currentStyle: currentStyle,
        currentVelocity: mapObject(_zero2['default'], currentStyle)
      };
    },

    componentDidMount: function componentDidMount() {
      this.startAnimating();
    },

    componentWillReceiveProps: function componentWillReceiveProps() {
      this.startAnimating();
    },

    animationStep: function animationStep(timestep, state) {
      var currentStyle = state.currentStyle;
      var currentVelocity = state.currentVelocity;
      var style = this.props.style;

      var newCurrentStyle = _updateTree.updateCurrentStyle(timestep, currentStyle, currentVelocity, style);
      var newCurrentVelocity = _updateTree.updateCurrentVelocity(timestep, currentStyle, currentVelocity, style);

      // TOOD: this isn't necessary anymore. It was used only against endValue func
      if (_noVelocity2['default'](currentVelocity, newCurrentStyle) && _noVelocity2['default'](newCurrentVelocity, newCurrentStyle)) {
        // check explanation in `Motion.animationRender`
        this.stopAnimation(); // Nasty side effects....
      }

      return {
        currentStyle: newCurrentStyle,
        currentVelocity: newCurrentVelocity
      };
    },

    stopAnimation: null,

    // used in animationRender
    hasUnmounted: false,

    componentWillUnmount: function componentWillUnmount() {
      this.stopAnimation();
      this.hasUnmounted = true;
    },

    startAnimating: function startAnimating() {
      // Is smart enough to not start it twice
      this.stopAnimation = startAnimation(this.state, this.animationStep, this.animationRender);
    },

    animationRender: function animationRender(alpha, nextState, prevState) {
      // `this.hasUnmounted` might be true in the following condition:
      // user does some checks in `style` and calls an owner handler
      // owner sets state in the callback, triggering a re-render
      // unmounts Motion
      if (!this.hasUnmounted) {
        this.setState({
          currentStyle: _updateTree.interpolateValue(alpha, nextState.currentStyle, prevState.currentStyle),
          currentVelocity: nextState.currentVelocity
        });
      }
    },

    render: function render() {
      var strippedStyle = _stripStyle2['default'](this.state.currentStyle);
      var renderedChildren = this.props.children(strippedStyle);
      return renderedChildren && React.Children.only(renderedChildren);
    }
  });

  var StaggeredMotion = React.createClass({
    displayName: 'StaggeredMotion',

    propTypes: {
      defaultStyle: function defaultStyle(prop, propName) {
        if (prop[propName]) {
          return new Error('You forgot the "s" for `StaggeredMotion`\'s `defaultStyles`.');
        }
      },
      style: function style(prop, propName) {
        if (prop[propName]) {
          return new Error('You forgot the "s" for `StaggeredMotion`\'s `styles`.');
        }
      },
      // TOOD: warn against putting configs in here
      defaultStyles: PropTypes.arrayOf(PropTypes.object),
      styles: PropTypes.func.isRequired,
      children: PropTypes.func.isRequired
    },

    getInitialState: function getInitialState() {
      var _props2 = this.props;
      var styles = _props2.styles;
      var defaultStyles = _props2.defaultStyles;

      var currentStyles = defaultStyles ? defaultStyles : styles();
      return {
        currentStyles: currentStyles,
        currentVelocities: currentStyles.map(function (s) {
          return mapObject(_zero2['default'], s);
        })
      };
    },

    componentDidMount: function componentDidMount() {
      this.startAnimating();
    },

    componentWillReceiveProps: function componentWillReceiveProps() {
      this.startAnimating();
    },

    animationStep: function animationStep(timestep, state) {
      var currentStyles = state.currentStyles;
      var currentVelocities = state.currentVelocities;

      var styles = this.props.styles(currentStyles.map(_stripStyle2['default']));

      var newCurrentStyles = currentStyles.map(function (currentStyle, i) {
        return _updateTree.updateCurrentStyle(timestep, currentStyle, currentVelocities[i], styles[i]);
      });
      var newCurrentVelocities = currentStyles.map(function (currentStyle, i) {
        return _updateTree.updateCurrentVelocity(timestep, currentStyle, currentVelocities[i], styles[i]);
      });

      // TODO: is this right?
      if (currentVelocities.every(function (v, k) {
        return _noVelocity2['default'](v, currentStyles[k]);
      }) && newCurrentVelocities.every(function (v, k) {
        return _noVelocity2['default'](v, newCurrentStyles[k]);
      })) {
        this.stopAnimation();
      }

      return {
        currentStyles: newCurrentStyles,
        currentVelocities: newCurrentVelocities
      };
    },

    stopAnimation: null,

    // used in animationRender
    hasUnmounted: false,

    componentWillUnmount: function componentWillUnmount() {
      this.stopAnimation();
      this.hasUnmounted = true;
    },

    startAnimating: function startAnimating() {
      this.stopAnimation = startAnimation(this.state, this.animationStep, this.animationRender);
    },

    animationRender: function animationRender(alpha, nextState, prevState) {
      // See comment in Motion.
      if (!this.hasUnmounted) {
        var currentStyles = nextState.currentStyles.map(function (style, i) {
          return _updateTree.interpolateValue(alpha, style, prevState.currentStyles[i]);
        });
        this.setState({
          currentStyles: currentStyles,
          currentVelocities: nextState.currentVelocities
        });
      }
    },

    render: function render() {
      var strippedStyle = this.state.currentStyles.map(_stripStyle2['default']);
      var renderedChildren = this.props.children(strippedStyle);
      return renderedChildren && React.Children.only(renderedChildren);
    }
  });

  var TransitionMotion = React.createClass({
    displayName: 'TransitionMotion',

    propTypes: {
      defaultValue: function defaultValue(prop, propName) {
        if (prop[propName]) {
          return new Error('TransitionSpring\'s `defaultValue` has been changed to ' + '`defaultStyles`. Its format received a few (easy to update!) ' + 'changes as well.');
        }
      },
      endValue: function endValue(prop, propName) {
        if (prop[propName]) {
          return new Error('TransitionSpring\'s `endValue` has been changed to `styles`. ' + 'Its format received a few (easy to update!) changes as well.');
        }
      },
      defaultStyle: function defaultStyle(prop, propName) {
        if (prop[propName]) {
          return new Error('You forgot the "s" for `TransitionMotion`\'s `defaultStyles`.');
        }
      },
      style: function style(prop, propName) {
        if (prop[propName]) {
          return new Error('You forgot the "s" for `TransitionMotion`\'s `styles`.');
        }
      },
      // TOOD: warn against putting configs in here
      defaultStyles: PropTypes.objectOf(PropTypes.any),
      styles: PropTypes.oneOfType([PropTypes.func, PropTypes.objectOf(PropTypes.any.isRequired)]).isRequired,
      willLeave: PropTypes.oneOfType([PropTypes.func]),
      // TOOD: warn against putting configs in here
      willEnter: PropTypes.oneOfType([PropTypes.func]),
      children: PropTypes.func.isRequired
    },

    getDefaultProps: function getDefaultProps() {
      return {
        willEnter: function willEnter(key, value) {
          return value;
        },
        willLeave: function willLeave() {
          return null;
        }
      };
    },

    getInitialState: function getInitialState() {
      var _props3 = this.props;
      var styles = _props3.styles;
      var defaultStyles = _props3.defaultStyles;

      var currentStyles = undefined;
      if (defaultStyles == null) {
        if (typeof styles === 'function') {
          currentStyles = styles();
        } else {
          currentStyles = styles;
        }
      } else {
        currentStyles = defaultStyles;
      }
      return {
        currentStyles: currentStyles,
        currentVelocities: mapObject(function (s) {
          return mapObject(_zero2['default'], s);
        }, currentStyles)
      };
    },

    componentDidMount: function componentDidMount() {
      this.startAnimating();
    },

    componentWillReceiveProps: function componentWillReceiveProps() {
      this.startAnimating();
    },

    animationStep: function animationStep(timestep, state) {
      var currentStyles = state.currentStyles;
      var currentVelocities = state.currentVelocities;
      var _props4 = this.props;
      var styles = _props4.styles;
      var willEnter = _props4.willEnter;
      var willLeave = _props4.willLeave;

      if (typeof styles === 'function') {
        styles = styles(currentStyles);
      }

      // TODO: huh?
      var mergedStyles = styles; // set mergedStyles to styles as the default
      var hasNewKey = false;

      mergedStyles = _mergeDiff2['default'](currentStyles, styles,
      // TODO: stop allocating like crazy in this whole code path
      function (key) {
        var res = willLeave(key, currentStyles[key], styles, currentStyles, currentVelocities);
        if (res == null) {
          // For legacy reason. We won't allow returning null soon
          // TODO: remove, after next release
          return null;
        }

        if (_noVelocity2['default'](currentVelocities[key], currentStyles[key]) && _hasReachedStyle2['default'](currentStyles[key], res)) {
          return null;
        }
        return res;
      });

      Object.keys(mergedStyles).filter(function (key) {
        return !currentStyles.hasOwnProperty(key);
      }).forEach(function (key) {
        var _extends2, _extends3;

        hasNewKey = true;
        var enterStyle = willEnter(key, mergedStyles[key], styles, currentStyles, currentVelocities);

        // We can mutate this here because mergeDiff returns a new Obj
        mergedStyles[key] = enterStyle;

        currentStyles = _extends({}, currentStyles, (_extends2 = {}, _extends2[key] = enterStyle, _extends2));
        currentVelocities = _extends({}, currentVelocities, (_extends3 = {}, _extends3[key] = mapObject(_zero2['default'], enterStyle), _extends3));
      });

      var newCurrentStyles = mapObject(function (mergedStyle, key) {
        return _updateTree.updateCurrentStyle(timestep, currentStyles[key], currentVelocities[key], mergedStyle);
      }, mergedStyles);
      var newCurrentVelocities = mapObject(function (mergedStyle, key) {
        return _updateTree.updateCurrentVelocity(timestep, currentStyles[key], currentVelocities[key], mergedStyle);
      }, mergedStyles);

      if (!hasNewKey && everyObj(function (v, k) {
        return _noVelocity2['default'](v, currentStyles[k]);
      }, currentVelocities) && everyObj(function (v, k) {
        return _noVelocity2['default'](v, newCurrentStyles[k]);
      }, newCurrentVelocities)) {
        // check explanation in `Motion.animationRender`
        this.stopAnimation(); // Nasty side effects....
      }

      return {
        currentStyles: newCurrentStyles,
        currentVelocities: newCurrentVelocities
      };
    },

    stopAnimation: null,

    // used in animationRender
    hasUnmounted: false,

    componentWillUnmount: function componentWillUnmount() {
      this.stopAnimation();
      this.hasUnmounted = true;
    },

    startAnimating: function startAnimating() {
      this.stopAnimation = startAnimation(this.state, this.animationStep, this.animationRender);
    },

    animationRender: function animationRender(alpha, nextState, prevState) {
      // See comment in Motion.
      if (!this.hasUnmounted) {
        var currentStyles = mapObject(function (style, key) {
          return _updateTree.interpolateValue(alpha, style, prevState.currentStyles[key]);
        }, nextState.currentStyles);
        this.setState({
          currentStyles: currentStyles,
          currentVelocities: nextState.currentVelocities
        });
      }
    },

    render: function render() {
      var strippedStyle = mapObject(_stripStyle2['default'], this.state.currentStyles);
      var renderedChildren = this.props.children(strippedStyle);
      return renderedChildren && React.Children.only(renderedChildren);
    }
  });

  var _deprecatedSprings = _deprecatedSprings3['default'](React);

  var Spring = _deprecatedSprings.Spring;
  var TransitionSpring = _deprecatedSprings.TransitionSpring;

  return { Spring: Spring, TransitionSpring: TransitionSpring, Motion: Motion, StaggeredMotion: StaggeredMotion, TransitionMotion: TransitionMotion };
}

module.exports = exports['default'];