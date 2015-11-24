(function webpackUniversalModuleDefinition(root, factory) {
	if(typeof exports === 'object' && typeof module === 'object')
		module.exports = factory(require("react"), require("react-motion"), require("react-height"));
	else if(typeof define === 'function' && define.amd)
		define(["react", "react-motion", "react-height"], factory);
	else if(typeof exports === 'object')
		exports["ReactCollapse"] = factory(require("react"), require("react-motion"), require("react-height"));
	else
		root["ReactCollapse"] = factory(root["React"], root["ReactMotion"], root["ReactHeight"]);
})(this, function(__WEBPACK_EXTERNAL_MODULE_2__, __WEBPACK_EXTERNAL_MODULE_7__, __WEBPACK_EXTERNAL_MODULE_8__) {
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
	
	Object.defineProperty(exports, '__esModule', {
	  value: true
	});
	
	function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { 'default': obj }; }
	
	var _Collapse = __webpack_require__(1);
	
	var _Collapse2 = _interopRequireDefault(_Collapse);

	exports['default'] = _Collapse2['default'];
	module.exports = exports['default'];

/***/ },
/* 1 */
/***/ function(module, exports, __webpack_require__) {

	'use strict';
	
	Object.defineProperty(exports, '__esModule', {
	  value: true
	});
	
	var _extends = Object.assign || function (target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i]; for (var key in source) { if (Object.prototype.hasOwnProperty.call(source, key)) { target[key] = source[key]; } } } return target; };
	
	function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { 'default': obj }; }
	
	function _objectWithoutProperties(obj, keys) { var target = {}; for (var i in obj) { if (keys.indexOf(i) >= 0) continue; if (!Object.prototype.hasOwnProperty.call(obj, i)) continue; target[i] = obj[i]; } return target; }
	
	var _react = __webpack_require__(2);
	
	var _react2 = _interopRequireDefault(_react);
	
	var _reactAddonsPureRenderMixin = __webpack_require__(3);
	
	var _reactMotion = __webpack_require__(7);
	
	var _reactHeight = __webpack_require__(8);
	
	var _reactHeight2 = _interopRequireDefault(_reactHeight);
	
	var Collapse = _react2['default'].createClass({
	  displayName: 'Collapse',
	
	  propTypes: {
	    isOpened: _react2['default'].PropTypes.bool.isRequired,
	    children: _react2['default'].PropTypes.node.isRequired,
	    fixedHeight: _react2['default'].PropTypes.number,
	    style: _react2['default'].PropTypes.object,
	    springConfig: _react2['default'].PropTypes.arrayOf(_react2['default'].PropTypes.number)
	  },
	
	  getDefaultProps: function getDefaultProps() {
	    return { fixedHeight: -1, style: {} };
	  },
	
	  getInitialState: function getInitialState() {
	    return { height: -1 };
	  },
	
	  componentWillMount: function componentWillMount() {
	    this.height = '0.0';
	  },
	
	  shouldComponentUpdate: _reactAddonsPureRenderMixin.shouldComponentUpdate,
	
	  onHeightReady: function onHeightReady(height) {
	    this.setState({ height: height });
	  },
	
	  renderFixed: function renderFixed() {
	    var _props = this.props;
	    var isOpened = _props.isOpened;
	    var style = _props.style;
	    var children = _props.children;
	    var fixedHeight = _props.fixedHeight;
	    var springConfig = _props.springConfig;
	
	    var props = _objectWithoutProperties(_props, ['isOpened', 'style', 'children', 'fixedHeight', 'springConfig']);
	
	    return _react2['default'].createElement(
	      _reactMotion.Motion,
	      {
	        defaultStyle: { height: 0 },
	        style: { height: (0, _reactMotion.spring)(isOpened ? fixedHeight : 0, springConfig) } },
	      function (_ref) {
	        var height = _ref.height;
	        return !isOpened && parseFloat(height).toFixed(1) === '0.0' ? null : _react2['default'].createElement(
	          'div',
	          _extends({ style: _extends({}, style, { height: height, overflow: 'hidden' }) }, props),
	          children
	        );
	      }
	    );
	  },
	
	  render: function render() {
	    var _this = this;
	
	    var _props2 = this.props;
	    var isOpened = _props2.isOpened;
	    var style = _props2.style;
	    var children = _props2.children;
	    var fixedHeight = _props2.fixedHeight;
	    var springConfig = _props2.springConfig;
	
	    var props = _objectWithoutProperties(_props2, ['isOpened', 'style', 'children', 'fixedHeight', 'springConfig']);
	
	    if (fixedHeight > -1) {
	      return this.renderFixed();
	    }
	
	    var height = this.state.height;
	
	    var stringHeight = parseFloat(height).toFixed(1);
	
	    // Cache Content so it is not re-rendered on each animation step
	    var content = _react2['default'].createElement(
	      _reactHeight2['default'],
	      { onHeightReady: this.onHeightReady },
	      children
	    );
	
	    return _react2['default'].createElement(
	      _reactMotion.Motion,
	      {
	        defaultStyle: { height: 0 },
	        style: { height: (0, _reactMotion.spring)(isOpened ? height : 0, springConfig) } },
	      function (st) {
	        _this.height = Math.max(0, parseFloat(st.height)).toFixed(1);
	
	        // TODO: this should be done using onEnd from ReactMotion, which is not yet implemented
	        // See https://github.com/chenglou/react-motion/issues/235
	        if (!isOpened && _this.height === '0.0') {
	          return null;
	        }
	
	        var newStyle = isOpened && _this.height === stringHeight ? { height: 'auto' } : {
	          height: st.height, overflow: 'hidden'
	        };
	
	        return _react2['default'].createElement(
	          'div',
	          _extends({ style: _extends({}, style, newStyle) }, props),
	          content
	        );
	      }
	    );
	  }
	});
	
	exports['default'] = Collapse;
	module.exports = exports['default'];

/***/ },
/* 2 */
/***/ function(module, exports) {

	module.exports = __WEBPACK_EXTERNAL_MODULE_2__;

/***/ },
/* 3 */
/***/ function(module, exports, __webpack_require__) {

	module.exports = __webpack_require__(4);

/***/ },
/* 4 */
/***/ function(module, exports, __webpack_require__) {

	/**
	 * Copyright 2013-2015, Facebook, Inc.
	 * All rights reserved.
	 *
	 * This source code is licensed under the BSD-style license found in the
	 * LICENSE file in the root directory of this source tree. An additional grant
	 * of patent rights can be found in the PATENTS file in the same directory.
	 *
	 * @providesModule ReactComponentWithPureRenderMixin
	 */
	
	'use strict';
	
	var shallowCompare = __webpack_require__(5);
	
	/**
	 * If your React component's render function is "pure", e.g. it will render the
	 * same result given the same props and state, provide this Mixin for a
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
/* 5 */
/***/ function(module, exports, __webpack_require__) {

	/**
	 * Copyright 2013-2015, Facebook, Inc.
	 * All rights reserved.
	 *
	 * This source code is licensed under the BSD-style license found in the
	 * LICENSE file in the root directory of this source tree. An additional grant
	 * of patent rights can be found in the PATENTS file in the same directory.
	 *
	* @providesModule shallowCompare
	*/
	
	'use strict';
	
	var shallowEqual = __webpack_require__(6);
	
	/**
	 * Does a shallow comparison for props and state.
	 * See ReactComponentWithPureRenderMixin
	 */
	function shallowCompare(instance, nextProps, nextState) {
	  return !shallowEqual(instance.props, nextProps) || !shallowEqual(instance.state, nextState);
	}
	
	module.exports = shallowCompare;

/***/ },
/* 6 */
/***/ function(module, exports) {

	/**
	 * Copyright 2013-2015, Facebook, Inc.
	 * All rights reserved.
	 *
	 * This source code is licensed under the BSD-style license found in the
	 * LICENSE file in the root directory of this source tree. An additional grant
	 * of patent rights can be found in the PATENTS file in the same directory.
	 *
	 * @providesModule shallowEqual
	 * @typechecks
	 * 
	 */
	
	'use strict';
	
	var hasOwnProperty = Object.prototype.hasOwnProperty;
	
	/**
	 * Performs equality by iterating through keys on an object and returning false
	 * when any key has values which are not strictly equal between the arguments.
	 * Returns true when the values of all keys are strictly equal.
	 */
	function shallowEqual(objA, objB) {
	  if (objA === objB) {
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
	  var bHasOwnProperty = hasOwnProperty.bind(objB);
	  for (var i = 0; i < keysA.length; i++) {
	    if (!bHasOwnProperty(keysA[i]) || objA[keysA[i]] !== objB[keysA[i]]) {
	      return false;
	    }
	  }
	
	  return true;
	}
	
	module.exports = shallowEqual;

/***/ },
/* 7 */
/***/ function(module, exports) {

	module.exports = __WEBPACK_EXTERNAL_MODULE_7__;

/***/ },
/* 8 */
/***/ function(module, exports) {

	module.exports = __WEBPACK_EXTERNAL_MODULE_8__;

/***/ }
/******/ ])
});
;
//# sourceMappingURL=react-collapse.js.map