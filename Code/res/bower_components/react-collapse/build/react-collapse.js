(function webpackUniversalModuleDefinition(root, factory) {
	if(typeof exports === 'object' && typeof module === 'object')
		module.exports = factory(require("react"), require("react-motion"), require("react-height"));
	else if(typeof define === 'function' && define.amd)
		define(["react", "react-motion", "react-height"], factory);
	else if(typeof exports === 'object')
		exports["ReactCollapse"] = factory(require("react"), require("react-motion"), require("react-height"));
	else
		root["ReactCollapse"] = factory(root["React"], root["ReactMotion"], root["ReactHeight"]);
})(this, function(__WEBPACK_EXTERNAL_MODULE_2__, __WEBPACK_EXTERNAL_MODULE_6__, __WEBPACK_EXTERNAL_MODULE_7__) {
return /******/ (function(modules) { // webpackBootstrap
/******/ 	// The module cache
/******/ 	var installedModules = {};
/******/
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/
/******/ 		// Check if module is in cache
/******/ 		if(installedModules[moduleId])
/******/ 			return installedModules[moduleId].exports;
/******/
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = installedModules[moduleId] = {
/******/ 			exports: {},
/******/ 			id: moduleId,
/******/ 			loaded: false
/******/ 		};
/******/
/******/ 		// Execute the module function
/******/ 		modules[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/
/******/ 		// Flag the module as loaded
/******/ 		module.loaded = true;
/******/
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/
/******/
/******/ 	// expose the modules object (__webpack_modules__)
/******/ 	__webpack_require__.m = modules;
/******/
/******/ 	// expose the module cache
/******/ 	__webpack_require__.c = installedModules;
/******/
/******/ 	// __webpack_public_path__
/******/ 	__webpack_require__.p = "";
/******/
/******/ 	// Load entry module and return exports
/******/ 	return __webpack_require__(0);
/******/ })
/************************************************************************/
/******/ ([
/* 0 */
/***/ function(module, exports, __webpack_require__) {

	'use strict';
	
	// Babel6 does not hack the default behaviour of ES Modules anymore, so we should export
	
	var Collapse = __webpack_require__(1).default;
	
	module.exports = Collapse;

/***/ },
/* 1 */
/***/ function(module, exports, __webpack_require__) {

	'use strict';
	
	Object.defineProperty(exports, "__esModule", {
	  value: true
	});
	
	var _extends = Object.assign || function (target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i]; for (var key in source) { if (Object.prototype.hasOwnProperty.call(source, key)) { target[key] = source[key]; } } } return target; };
	
	var _react = __webpack_require__(2);
	
	var _react2 = _interopRequireDefault(_react);
	
	var _ReactComponentWithPureRenderMixin = __webpack_require__(3);
	
	var _reactMotion = __webpack_require__(6);
	
	var _reactHeight = __webpack_require__(7);
	
	var _reactHeight2 = _interopRequireDefault(_reactHeight);
	
	function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }
	
	function _objectWithoutProperties(obj, keys) { var target = {}; for (var i in obj) { if (keys.indexOf(i) >= 0) continue; if (!Object.prototype.hasOwnProperty.call(obj, i)) continue; target[i] = obj[i]; } return target; }
	
	var stringHeight = function stringHeight(height) {
	  return Math.max(0, parseFloat(height)).toFixed(1);
	};
	
	var Collapse = _react2.default.createClass({
	  displayName: 'Collapse',
	
	  propTypes: {
	    isOpened: _react2.default.PropTypes.bool.isRequired,
	    children: _react2.default.PropTypes.node.isRequired,
	    fixedHeight: _react2.default.PropTypes.number,
	    style: _react2.default.PropTypes.object, // eslint-disable-line react/forbid-prop-types
	    springConfig: _react2.default.PropTypes.objectOf(_react2.default.PropTypes.number),
	    keepCollapsedContent: _react2.default.PropTypes.bool
	  },
	
	  getDefaultProps: function getDefaultProps() {
	    return { fixedHeight: -1, style: {}, keepCollapsedContent: false };
	  },
	  getInitialState: function getInitialState() {
	    return { height: -1, isOpenedChanged: false };
	  },
	  componentWillMount: function componentWillMount() {
	    this.height = stringHeight(0);
	    this.renderStatic = true;
	  },
	  componentWillReceiveProps: function componentWillReceiveProps(_ref) {
	    var isOpened = _ref.isOpened;
	
	    this.setState({ isOpenedChanged: isOpened !== this.props.isOpened });
	  },
	
	
	  shouldComponentUpdate: _ReactComponentWithPureRenderMixin.shouldComponentUpdate,
	
	  onHeightReady: function onHeightReady(height) {
	    if (this.renderStatic && this.props.isOpened) {
	      this.height = stringHeight(height);
	    }
	    this.setState({ height: height });
	  },
	  getMotionHeight: function getMotionHeight(height) {
	    var _props = this.props;
	    var isOpened = _props.isOpened;
	    var springConfig = _props.springConfig;
	    var isOpenedChanged = this.state.isOpenedChanged;
	
	
	    var newHeight = isOpened ? Math.max(0, parseFloat(height)).toFixed(1) : stringHeight(0);
	
	    // No need to animate if content is closed and it was closed previously
	    // Also no need to animate if height did not change
	    var skipAnimation = !isOpenedChanged && !isOpened || this.height === newHeight;
	
	    var springHeight = (0, _reactMotion.spring)(isOpened ? Math.max(0, height) : 0, springConfig);
	    var instantHeight = isOpened ? Math.max(0, height) : 0;
	
	    return skipAnimation ? instantHeight : springHeight;
	  },
	  renderFixed: function renderFixed() {
	    var _this = this;
	
	    var _props2 = this.props;
	    var isOpened = _props2.isOpened;
	    var style = _props2.style;
	    var children = _props2.children;
	    var fixedHeight = _props2.fixedHeight;
	    var _ = _props2.springConfig;
	    var keepCollapsedContent = _props2.keepCollapsedContent;
	
	    var props = _objectWithoutProperties(_props2, ['isOpened', 'style', 'children', 'fixedHeight', 'springConfig', 'keepCollapsedContent']);
	
	    if (this.renderStatic) {
	      this.renderStatic = false;
	      var newStyle = { overflow: 'hidden', height: isOpened ? fixedHeight : 0 };
	
	      if (!keepCollapsedContent && !isOpened) {
	        return null;
	      }
	      this.height = stringHeight(fixedHeight);
	      return _react2.default.createElement(
	        'div',
	        _extends({ style: _extends({}, newStyle, style) }, props),
	        children
	      );
	    }
	
	    return _react2.default.createElement(
	      _reactMotion.Motion,
	      {
	        defaultStyle: { height: isOpened ? 0 : fixedHeight },
	        style: { height: this.getMotionHeight(fixedHeight) } },
	      function (_ref2) {
	        var height = _ref2.height;
	
	        _this.height = stringHeight(height);
	
	        // TODO: this should be done using onEnd from ReactMotion, which is not yet implemented
	        // See https://github.com/chenglou/react-motion/issues/235
	        if (!keepCollapsedContent && !isOpened && _this.height === stringHeight(0)) {
	          return null;
	        }
	
	        var newStyle = { overflow: 'hidden', height: height };
	
	        return _react2.default.createElement(
	          'div',
	          _extends({ style: _extends({}, newStyle, style) }, props),
	          children
	        );
	      }
	    );
	  },
	  render: function render() {
	    var _this2 = this;
	
	    var _props3 = this.props;
	    var isOpened = _props3.isOpened;
	    var style = _props3.style;
	    var children = _props3.children;
	    var fixedHeight = _props3.fixedHeight;
	    var _ = _props3.springConfig;
	    var keepCollapsedContent = _props3.keepCollapsedContent;
	
	    var props = _objectWithoutProperties(_props3, ['isOpened', 'style', 'children', 'fixedHeight', 'springConfig', 'keepCollapsedContent']);
	
	    if (fixedHeight > -1) {
	      return this.renderFixed();
	    }
	
	    var renderStatic = this.renderStatic;
	    var height = this.state.height;
	
	    var currentStringHeight = parseFloat(height).toFixed(1);
	
	    if (height > -1 && renderStatic) {
	      this.renderStatic = false;
	    }
	
	    // Cache Content so it is not re-rendered on each animation step
	    var content = _react2.default.createElement(
	      _reactHeight2.default,
	      { onHeightReady: this.onHeightReady },
	      children
	    );
	
	    if (renderStatic) {
	      var newStyle = { overflow: 'hidden', height: isOpened ? 'auto' : 0 };
	
	      if (!isOpened && height > -1) {
	        if (!keepCollapsedContent) {
	          return null;
	        }
	        return _react2.default.createElement(
	          'div',
	          _extends({ style: _extends({ height: 0, overflow: 'hidden' }, style) }, props),
	          content
	        );
	      }
	
	      return _react2.default.createElement(
	        'div',
	        _extends({ style: _extends({}, newStyle, style) }, props),
	        content
	      );
	    }
	
	    return _react2.default.createElement(
	      _reactMotion.Motion,
	      {
	        defaultStyle: { height: isOpened ? 0 : Math.max(0, height) },
	        style: { height: this.getMotionHeight(height) } },
	      function (st) {
	        _this2.height = stringHeight(st.height);
	
	        // TODO: this should be done using onEnd from ReactMotion, which is not yet implemented
	        // See https://github.com/chenglou/react-motion/issues/235
	        if (!isOpened && _this2.height === '0.0') {
	          if (!keepCollapsedContent) {
	            return null;
	          }
	          return _react2.default.createElement(
	            'div',
	            _extends({ style: _extends({ height: 0, overflow: 'hidden' }, style) }, props),
	            content
	          );
	        }
	
	        var newStyle = isOpened && _this2.height === currentStringHeight ? { height: 'auto' } : {
	          height: st.height, overflow: 'hidden'
	        };
	
	        return _react2.default.createElement(
	          'div',
	          _extends({ style: _extends({}, newStyle, style) }, props),
	          content
	        );
	      }
	    );
	  }
	});
	
	exports.default = Collapse;

/***/ },
/* 2 */
/***/ function(module, exports) {

	module.exports = __WEBPACK_EXTERNAL_MODULE_2__;

/***/ },
/* 3 */
/***/ function(module, exports, __webpack_require__) {

	/**
	 * Copyright 2013-present, Facebook, Inc.
	 * All rights reserved.
	 *
	 * This source code is licensed under the BSD-style license found in the
	 * LICENSE file in the root directory of this source tree. An additional grant
	 * of patent rights can be found in the PATENTS file in the same directory.
	 *
	 * @providesModule ReactComponentWithPureRenderMixin
	 */
	
	'use strict';
	
	var shallowCompare = __webpack_require__(4);
	
	/**
	 * If your React component's render function is "pure", e.g. it will render the
	 * same result given the same props and state, provide this mixin for a
	 * considerable performance boost.
	 *
	 * Most React components have pure render functions.
	 *
	 * Example:
	 *
	 *   var ReactComponentWithPureRenderMixin =
	 *     require('ReactComponentWithPureRenderMixin');
	 *   React.createClass({
	 *     mixins: [ReactComponentWithPureRenderMixin],
	 *
	 *     render: function() {
	 *       return <div className={this.props.className}>foo</div>;
	 *     }
	 *   });
	 *
	 * Note: This only checks shallow equality for props and state. If these contain
	 * complex data structures this mixin may have false-negatives for deeper
	 * differences. Only mixin to components which have simple props and state, or
	 * use `forceUpdate()` when you know deep data structures have changed.
	 */
	var ReactComponentWithPureRenderMixin = {
	  shouldComponentUpdate: function (nextProps, nextState) {
	    return shallowCompare(this, nextProps, nextState);
	  }
	};
	
	module.exports = ReactComponentWithPureRenderMixin;

/***/ },
/* 4 */
/***/ function(module, exports, __webpack_require__) {

	/**
	 * Copyright 2013-present, Facebook, Inc.
	 * All rights reserved.
	 *
	 * This source code is licensed under the BSD-style license found in the
	 * LICENSE file in the root directory of this source tree. An additional grant
	 * of patent rights can be found in the PATENTS file in the same directory.
	 *
	* @providesModule shallowCompare
	*/
	
	'use strict';
	
	var shallowEqual = __webpack_require__(5);
	
	/**
	 * Does a shallow comparison for props and state.
	 * See ReactComponentWithPureRenderMixin
	 */
	function shallowCompare(instance, nextProps, nextState) {
	  return !shallowEqual(instance.props, nextProps) || !shallowEqual(instance.state, nextState);
	}
	
	module.exports = shallowCompare;

/***/ },
/* 5 */
/***/ function(module, exports) {

	/**
	 * Copyright (c) 2013-present, Facebook, Inc.
	 * All rights reserved.
	 *
	 * This source code is licensed under the BSD-style license found in the
	 * LICENSE file in the root directory of this source tree. An additional grant
	 * of patent rights can be found in the PATENTS file in the same directory.
	 *
	 * @typechecks
	 * 
	 */
	
	/*eslint-disable no-self-compare */
	
	'use strict';
	
	var hasOwnProperty = Object.prototype.hasOwnProperty;
	
	/**
	 * inlined Object.is polyfill to avoid requiring consumers ship their own
	 * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/is
	 */
	function is(x, y) {
	  // SameValue algorithm
	  if (x === y) {
	    // Steps 1-5, 7-10
	    // Steps 6.b-6.e: +0 != -0
	    return x !== 0 || 1 / x === 1 / y;
	  } else {
	    // Step 6.a: NaN == NaN
	    return x !== x && y !== y;
	  }
	}
	
	/**
	 * Performs equality by iterating through keys on an object and returning false
	 * when any key has values which are not strictly equal between the arguments.
	 * Returns true when the values of all keys are strictly equal.
	 */
	function shallowEqual(objA, objB) {
	  if (is(objA, objB)) {
	    return true;
	  }
	
	  if (typeof objA !== 'object' || objA === null || typeof objB !== 'object' || objB === null) {
	    return false;
	  }
	
	  var keysA = Object.keys(objA);
	  var keysB = Object.keys(objB);
	
	  if (keysA.length !== keysB.length) {
	    return false;
	  }
	
	  // Test for A's keys different from B.
	  for (var i = 0; i < keysA.length; i++) {
	    if (!hasOwnProperty.call(objB, keysA[i]) || !is(objA[keysA[i]], objB[keysA[i]])) {
	      return false;
	    }
	  }
	
	  return true;
	}
	
	module.exports = shallowEqual;

/***/ },
/* 6 */
/***/ function(module, exports) {

	module.exports = __WEBPACK_EXTERNAL_MODULE_6__;

/***/ },
/* 7 */
/***/ function(module, exports) {

	module.exports = __WEBPACK_EXTERNAL_MODULE_7__;

/***/ }
/******/ ])
});
;
//# sourceMappingURL=react-collapse.js.map