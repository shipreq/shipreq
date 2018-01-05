/******/ (function(modules) { // webpackBootstrap
/******/ 	// The module cache
/******/ 	var installedModules = {};
/******/
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/
/******/ 		// Check if module is in cache
/******/ 		if(installedModules[moduleId]) {
/******/ 			return installedModules[moduleId].exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = installedModules[moduleId] = {
/******/ 			i: moduleId,
/******/ 			l: false,
/******/ 			exports: {}
/******/ 		};
/******/
/******/ 		// Execute the module function
/******/ 		modules[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/
/******/ 		// Flag the module as loaded
/******/ 		module.l = true;
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
/******/ 	// define getter function for harmony exports
/******/ 	__webpack_require__.d = function(exports, name, getter) {
/******/ 		if(!__webpack_require__.o(exports, name)) {
/******/ 			Object.defineProperty(exports, name, {
/******/ 				configurable: false,
/******/ 				enumerable: true,
/******/ 				get: getter
/******/ 			});
/******/ 		}
/******/ 	};
/******/
/******/ 	// getDefaultExport function for compatibility with non-harmony modules
/******/ 	__webpack_require__.n = function(module) {
/******/ 		var getter = module && module.__esModule ?
/******/ 			function getDefault() { return module['default']; } :
/******/ 			function getModuleExports() { return module; };
/******/ 		__webpack_require__.d(getter, 'a', getter);
/******/ 		return getter;
/******/ 	};
/******/
/******/ 	// Object.prototype.hasOwnProperty.call
/******/ 	__webpack_require__.o = function(object, property) { return Object.prototype.hasOwnProperty.call(object, property); };
/******/
/******/ 	// __webpack_public_path__
/******/ 	__webpack_require__.p = "";
/******/
/******/ 	// Load entry module and return exports
/******/ 	return __webpack_require__(__webpack_require__.s = 185);
/******/ })
/************************************************************************/
/******/ ({

/***/ 10:
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "d", function() { return VERSION; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "a", function() { return DEV_ID; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "e", function() { return VERSION_PARAM; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "c", function() { return USAGE_PARAM; });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "b", function() { return NULL_DIMENSION; });
/**
 * Copyright 2016 Google Inc. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

var VERSION = '2.4.1';
var DEV_ID = 'i5iSjo';

var VERSION_PARAM = '_av';
var USAGE_PARAM = '_au';

var NULL_DIMENSION = '(not set)';

/***/ }),

/***/ 11:
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

/**
 * Copyright 2017 Google Inc. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
 * @fileoverview
 * The functions exported by this module make it easier (and safer) to override
 * foreign object methods (in a modular way) and respond to or modify their
 * invocation. The primary feature is the ability to override a method without
 * worrying if it's already been overridden somewhere else in the codebase. It
 * also allows for safe restoring of an overridden method by only fully
 * restoring a method once all overrides have been removed.
 */

var instances = [];

/**
 * A class that wraps a foreign object method and emit events before and
 * after the original method is called.
 */

var MethodChain = function () {
  _createClass(MethodChain, null, [{
    key: "add",

    /**
     * Adds the passed override method to the list of method chain overrides.
     * @param {!Object} context The object containing the method to chain.
     * @param {string} methodName The name of the method on the object.
     * @param {!Function} methodOverride The override method to add.
     */
    value: function add(context, methodName, methodOverride) {
      getOrCreateMethodChain(context, methodName).add(methodOverride);
    }

    /**
     * Removes a method chain added via `add()`. If the override is the
     * only override added, the original method is restored.
     * @param {!Object} context The object containing the method to unchain.
     * @param {string} methodName The name of the method on the object.
     * @param {!Function} methodOverride The override method to remove.
     */

  }, {
    key: "remove",
    value: function remove(context, methodName, methodOverride) {
      getOrCreateMethodChain(context, methodName).remove(methodOverride);
    }

    /**
     * Wraps a foreign object method and overrides it. Also stores a reference
     * to the original method so it can be restored later.
     * @param {!Object} context The object containing the method.
     * @param {string} methodName The name of the method on the object.
     */

  }]);

  function MethodChain(context, methodName) {
    var _this = this;

    _classCallCheck(this, MethodChain);

    this.context = context;
    this.methodName = methodName;
    this.isTask = /Task$/.test(methodName);

    this.originalMethodReference = this.isTask ? context.get(methodName) : context[methodName];

    this.methodChain = [];
    this.boundMethodChain = [];

    // Wraps the original method.
    this.wrappedMethod = function () {
      var lastBoundMethod = _this.boundMethodChain[_this.boundMethodChain.length - 1];

      return lastBoundMethod.apply(undefined, arguments);
    };

    // Override original method with the wrapped one.
    if (this.isTask) {
      context.set(methodName, this.wrappedMethod);
    } else {
      context[methodName] = this.wrappedMethod;
    }
  }

  /**
   * Adds a method to the method chain.
   * @param {!Function} overrideMethod The override method to add.
   */


  _createClass(MethodChain, [{
    key: "add",
    value: function add(overrideMethod) {
      this.methodChain.push(overrideMethod);
      this.rebindMethodChain();
    }

    /**
     * Removes a method from the method chain and restores the prior order.
     * @param {!Function} overrideMethod The override method to remove.
     */

  }, {
    key: "remove",
    value: function remove(overrideMethod) {
      var index = this.methodChain.indexOf(overrideMethod);
      if (index > -1) {
        this.methodChain.splice(index, 1);
        if (this.methodChain.length > 0) {
          this.rebindMethodChain();
        } else {
          this.destroy();
        }
      }
    }

    /**
     * Loops through the method chain array and recreates the bound method
     * chain array. This is necessary any time a method is added or removed
     * to ensure proper original method context and order.
     */

  }, {
    key: "rebindMethodChain",
    value: function rebindMethodChain() {
      this.boundMethodChain = [];
      for (var method, i = 0; method = this.methodChain[i]; i++) {
        var previousMethod = this.boundMethodChain[i - 1] || this.originalMethodReference.bind(this.context);
        this.boundMethodChain.push(method(previousMethod));
      }
    }

    /**
     * Calls super and destroys the instance if no registered handlers remain.
     */

  }, {
    key: "destroy",
    value: function destroy() {
      var index = instances.indexOf(this);
      if (index > -1) {
        instances.splice(index, 1);
        if (this.isTask) {
          this.context.set(this.methodName, this.originalMethodReference);
        } else {
          this.context[this.methodName] = this.originalMethodReference;
        }
      }
    }
  }]);

  return MethodChain;
}();

/**
 * Gets a MethodChain instance for the passed object and method. If the method
 * has already been wrapped via an existing MethodChain instance, that
 * instance is returned.
 * @param {!Object} context The object containing the method.
 * @param {string} methodName The name of the method on the object.
 * @return {!MethodChain}
 */


/* harmony default export */ __webpack_exports__["a"] = (MethodChain);
function getOrCreateMethodChain(context, methodName) {
  var methodChain = instances.filter(function (h) {
    return h.context == context && h.methodName == methodName;
  })[0];

  if (!methodChain) {
    methodChain = new MethodChain(context, methodName);
    instances.push(methodChain);
  }
  return methodChain;
}

/***/ }),

/***/ 146:
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = closest;

var _matches = __webpack_require__(21);

var _matches2 = _interopRequireDefault(_matches);

var _parents = __webpack_require__(147);

var _parents2 = _interopRequireDefault(_parents);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * Gets the closest parent element that matches the passed selector.
 * @param {Element} element The element whose parents to check.
 * @param {string} selector The CSS selector to match against.
 * @param {boolean=} shouldCheckSelf True if the selector should test against
 *     the passed element itself.
 * @return {Element|undefined} The matching element or undefined.
 */
function closest(element, selector) {
  var shouldCheckSelf = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : false;

  if (!(element && element.nodeType == 1 && selector)) return;
  var parentElements = (shouldCheckSelf ? [element] : []).concat((0, _parents2.default)(element));

  for (var i = 0, parent; parent = parentElements[i]; i++) {
    if ((0, _matches2.default)(parent, selector)) return parent;
  }
}

/***/ }),

/***/ 147:
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = parents;
/**
 * Returns an array of a DOM element's parent elements.
 * @param {!Element} element The DOM element whose parents to get.
 * @return {!Array} An array of all parent elemets, or an empty array if no
 *     parent elements are found.
 */
function parents(element) {
  var list = [];
  while (element && element.parentNode && element.parentNode.nodeType == 1) {
    element = /** @type {!Element} */element.parentNode;
    list.push(element);
  }
  return list;
}

/***/ }),

/***/ 148:
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0__event_emitter__ = __webpack_require__(194);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1__utilities__ = __webpack_require__(3);
var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; }

/**
 * Copyright 2016 Google Inc. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */




var AUTOTRACK_PREFIX = 'autotrack';
var instances = {};
var isListening = false;

/** @type {boolean|undefined} */
var browserSupportsLocalStorage = void 0;

/**
 * A storage object to simplify interacting with localStorage.
 */

var Store = function (_EventEmitter) {
  _inherits(Store, _EventEmitter);

  _createClass(Store, null, [{
    key: 'getOrCreate',

    /**
     * Gets an existing instance for the passed arguements or creates a new
     * instance if one doesn't exist.
     * @param {string} trackingId The tracking ID for the GA property.
     * @param {string} namespace A namespace unique to this store.
     * @param {Object=} defaults An optional object of key/value defaults.
     * @return {Store} The Store instance.
     */
    value: function getOrCreate(trackingId, namespace, defaults) {
      var key = [AUTOTRACK_PREFIX, trackingId, namespace].join(':');

      // Don't create multiple instances for the same tracking Id and namespace.
      if (!instances[key]) {
        instances[key] = new Store(key, defaults);
        if (!isListening) initStorageListener();
      }
      return instances[key];
    }

    /**
     * Returns true if the browser supports and can successfully write to
     * localStorage. The results is cached so this method can be invoked many
     * times with no extra performance cost.
     * @private
     * @return {boolean}
     */

  }, {
    key: 'isSupported_',
    value: function isSupported_() {
      if (browserSupportsLocalStorage != null) {
        return browserSupportsLocalStorage;
      }

      try {
        window.localStorage.setItem(AUTOTRACK_PREFIX, AUTOTRACK_PREFIX);
        window.localStorage.removeItem(AUTOTRACK_PREFIX);
        browserSupportsLocalStorage = true;
      } catch (err) {
        browserSupportsLocalStorage = false;
      }
      return browserSupportsLocalStorage;
    }

    /**
     * Wraps the native localStorage method for each stubbing in tests.
     * @private
     * @param {string} key The store key.
     * @return {string|null} The stored value.
     */

  }, {
    key: 'get_',
    value: function get_(key) {
      return window.localStorage.getItem(key);
    }

    /**
     * Wraps the native localStorage method for each stubbing in tests.
     * @private
     * @param {string} key The store key.
     * @param {string} value The value to store.
     */

  }, {
    key: 'set_',
    value: function set_(key, value) {
      window.localStorage.setItem(key, value);
    }

    /**
     * Wraps the native localStorage method for each stubbing in tests.
     * @private
     * @param {string} key The store key.
     */

  }, {
    key: 'clear_',
    value: function clear_(key) {
      window.localStorage.removeItem(key);
    }

    /**
     * @param {string} key A key unique to this store.
     * @param {Object=} defaults An optional object of key/value defaults.
     */

  }]);

  function Store(key) {
    var defaults = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {};

    _classCallCheck(this, Store);

    var _this = _possibleConstructorReturn(this, (Store.__proto__ || Object.getPrototypeOf(Store)).call(this));

    _this.key_ = key;
    _this.defaults_ = defaults;

    /** @type {?Object} */
    _this.cache_ = null; // Will be set after the first get.
    return _this;
  }

  /**
   * Gets the data stored in localStorage for this store. If the cache is
   * already populated, return it as is (since it's always kept up-to-date
   * and in sync with activity in other windows via the `storage` event).
   * TODO(philipwalton): Implement schema migrations if/when a new
   * schema version is introduced.
   * @return {!Object} The stored data merged with the defaults.
   */


  _createClass(Store, [{
    key: 'get',
    value: function get() {
      if (this.cache_) {
        return this.cache_;
      } else {
        if (Store.isSupported_()) {
          try {
            this.cache_ = parse(Store.get_(this.key_));
          } catch (err) {
            // Do nothing.
          }
        }
        return this.cache_ = Object(__WEBPACK_IMPORTED_MODULE_1__utilities__["a" /* assign */])({}, this.defaults_, this.cache_);
      }
    }

    /**
     * Saves the passed data object to localStorage,
     * merging it with the existing data.
     * @param {Object} newData The data to save.
     */

  }, {
    key: 'set',
    value: function set(newData) {
      this.cache_ = Object(__WEBPACK_IMPORTED_MODULE_1__utilities__["a" /* assign */])({}, this.defaults_, this.cache_, newData);

      if (Store.isSupported_()) {
        try {
          Store.set_(this.key_, JSON.stringify(this.cache_));
        } catch (err) {
          // Do nothing.
        }
      }
    }

    /**
     * Clears the data in localStorage for the current store.
     */

  }, {
    key: 'clear',
    value: function clear() {
      this.cache_ = {};
      if (Store.isSupported_()) {
        try {
          Store.clear_(this.key_);
        } catch (err) {
          // Do nothing.
        }
      }
    }

    /**
     * Removes the store instance for the global instances map. If this is the
     * last store instance, the storage listener is also removed.
     * Note: this does not erase the stored data. Use `clear()` for that.
     */

  }, {
    key: 'destroy',
    value: function destroy() {
      delete instances[this.key_];
      if (!Object.keys(instances).length) {
        removeStorageListener();
      }
    }
  }]);

  return Store;
}(__WEBPACK_IMPORTED_MODULE_0__event_emitter__["a" /* default */]);

/**
 * Adds a single storage event listener and flips the global `isListening`
 * flag so multiple events aren't added.
 */


/* harmony default export */ __webpack_exports__["a"] = (Store);
function initStorageListener() {
  window.addEventListener('storage', storageListener);
  isListening = true;
}

/**
 * Removes the storage event listener and flips the global `isListening`
 * flag so it can be re-added later.
 */
function removeStorageListener() {
  window.removeEventListener('storage', storageListener);
  isListening = false;
}

/**
 * The global storage event listener.
 * @param {!Event} event The DOM event.
 */
function storageListener(event) {
  var store = instances[event.key];
  if (store) {
    var oldData = Object(__WEBPACK_IMPORTED_MODULE_1__utilities__["a" /* assign */])({}, store.defaults_, parse(event.oldValue));
    var newData = Object(__WEBPACK_IMPORTED_MODULE_1__utilities__["a" /* assign */])({}, store.defaults_, parse(event.newValue));

    store.cache_ = newData;
    store.emit('externalSet', newData, oldData);
  }
}

/**
 * Parses a source string as JSON
 * @param {string|null} source
 * @return {!Object} The JSON object.
 */
function parse(source) {
  var data = {};
  if (source) {
    try {
      data = /** @type {!Object} */JSON.parse(source);
    } catch (err) {
      // Do nothing.
    }
  }
  return data;
}

/***/ }),

/***/ 185:
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
Object.defineProperty(__webpack_exports__, "__esModule", { value: true });
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "trackError", function() { return trackError; });
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_autotrack_lib_plugins_clean_url_tracker__ = __webpack_require__(186);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1_autotrack_lib_plugins_outbound_link_tracker__ = __webpack_require__(191);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2_autotrack_lib_plugins_page_visibility_tracker__ = __webpack_require__(192);
function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

/*
Originally copied from: https://github.com/philipwalton/analyticsjs-boilerplate/blob/master/src/analytics/autotrack.js
Commit: c6717f3d99650cd5faebb7b94ad27eb9510e62c1

See https://philipwalton.com/articles/the-google-analytics-setup-i-use-on-every-site-i-build/
*/

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// ShipReq-specific





/**
 * Bump this when making backwards incompatible changes to the tracking
 * implementation. This allows you to create a segment or view filter
 * that isolates only data captured with the most recent tracking changes.
 */
var TRACKING_VERSION = '1';

var TIMEZONE = 'Australia/Sydney';

// A mapping between custom dimension names and their indexes.
var dimensions = {
  TRACKING_VERSION: 'dimension1',
  CLIENT_ID: 'dimension2',
  WINDOW_ID: 'dimension3',
  HIT_ID: 'dimension4',
  HIT_TIME: 'dimension5',
  HIT_TYPE: 'dimension6',
  HIT_SOURCE: 'dimension7',
  VISIBILITY_STATE: 'dimension8'
};

// A mapping between custom metric names and their indexes.
var metrics = {
  RESPONSE_END_TIME: 'metric1',
  DOM_LOAD_TIME: 'metric2',
  WINDOW_LOAD_TIME: 'metric3',
  PAGE_VISIBLE: 'metric4',
  PAGE_LOADS: 'metric5'
};

var requireAutotrackPlugins = function requireAutotrackPlugins(sendPageviewNow) {

  ga('require', 'cleanUrlTracker', {
    stripQuery: true,
    trailingSlash: 'remove'
  });

  ga('require', 'outboundLinkTracker', {
    // Capture: Left-click, Middle-click, Right-click
    events: ['click', 'auxclick', 'contextmenu']
  });

  ga('require', 'pageVisibilityTracker', {
    sendInitialPageview: sendPageviewNow, // replaces ga('send','pageview') when true
    pageLoadsMetricIndex: getDefinitionIndex(metrics.PAGE_LOADS),
    visibleMetricIndex: getDefinitionIndex(metrics.PAGE_VISIBLE),
    timeZone: TIMEZONE,
    fieldsObj: _defineProperty({}, dimensions.HIT_SOURCE, 'pageVisibilityTracker')
  });
};

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// Generic, mostly identical with original source

/* global ga */

/**
 * A default value for dimensions so unset values always are reported as
 * something. This is needed since Google Analytics will drop empty dimension
 * values in reports.
 */
var NULL_VALUE = '(not set)';

/**
 * Initializes all the analytics setup. Creates trackers and sets initial
 * values on the trackers.
 */
var init = function init(TRACKING_ID, sendPageviewNow) {
  // Initialize the command queue in case analytics.js hasn't loaded yet.
  // window.ga = window.ga || ((...args) => (ga.q = ga.q || []).push(args));
  // ↑ no need for this; it's been added to the Lift snippet, Analytics.scala

  createTracker(TRACKING_ID);
  trackErrors();
  trackCustomDimensions();
  requireAutotrackPlugins(sendPageviewNow);
  sendNavigationTimingMetrics();
};

/**
 * Tracks a JavaScript error with optional fields object overrides.
 * This function is exported so it can be used in other parts of the codebase.
 * E.g.:
 *
 *    `fetch('/api.json').catch(trackError);`
 *
 * @param {(Error|Object)=} err
 * @param {Object=} fieldsObj
 */
var trackError = function trackError() {
  var err = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};
  var fieldsObj = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {};

  ga('send', 'event', Object.assign({
    eventCategory: 'Error',
    eventAction: err.name || '(no error name)',
    eventLabel: err.message + '\n' + (err.stack || '(no stack trace)'),
    nonInteraction: true
  }, fieldsObj));
};

/**
 * Creates the trackers and sets the default transport and tracking
 * version fields. In non-production environments it also logs hits.
 */
var createTracker = function createTracker(TRACKING_ID) {
  ga('create', TRACKING_ID, {
    // 100% of users will send site speed stats
    'siteSpeedSampleRate': 100
  });

  // Ensures all hits are sent via `navigator.sendBeacon()`.
  ga('set', 'transport', 'beacon');
};

/**
 * Tracks any errors that may have occured on the page prior to analytics being
 * initialized, then adds an event handler to track future errors.
 */
var trackErrors = function trackErrors() {
  // Errors that have occurred prior to this script running are stored on
  // `window.__e.q`, as specified in `index.html`.
  var loadErrorEvents = window.__e && window.__e.q || [];

  var trackErrorEvent = function trackErrorEvent(event) {
    // Use a different eventCategory for uncaught errors.
    var fieldsObj = { eventCategory: 'Uncaught Error' };

    // Some browsers don't have an error property, so we fake it.
    var err = event.error || {
      message: event.message + ' (' + event.lineno + ':' + event.colno + ')'
    };

    trackError(err, fieldsObj);
  };

  // Replay any stored load error events.
  var _iteratorNormalCompletion = true;
  var _didIteratorError = false;
  var _iteratorError = undefined;

  try {
    for (var _iterator = loadErrorEvents[Symbol.iterator](), _step; !(_iteratorNormalCompletion = (_step = _iterator.next()).done); _iteratorNormalCompletion = true) {
      var event = _step.value;

      trackErrorEvent(event);
    }

    // Add a new listener to track event immediately.
  } catch (err) {
    _didIteratorError = true;
    _iteratorError = err;
  } finally {
    try {
      if (!_iteratorNormalCompletion && _iterator.return) {
        _iterator.return();
      }
    } finally {
      if (_didIteratorError) {
        throw _iteratorError;
      }
    }
  }

  window.addEventListener('error', trackErrorEvent);
};

/**
 * Sets a default dimension value for all custom dimensions on all trackers.
 */
var trackCustomDimensions = function trackCustomDimensions() {
  // Sets a default dimension value for all custom dimensions to ensure
  // that every dimension in every hit has *some* value. This is necessary
  // because Google Analytics will drop rows with empty dimension values
  // in your reports.
  Object.keys(dimensions).forEach(function (key) {
    ga('set', dimensions[key], NULL_VALUE);
  });

  // Adds tracking of dimensions known at page load time.
  ga(function (tracker) {
    var _tracker$set;

    tracker.set((_tracker$set = {}, _defineProperty(_tracker$set, dimensions.TRACKING_VERSION, TRACKING_VERSION), _defineProperty(_tracker$set, dimensions.CLIENT_ID, tracker.get('clientId')), _defineProperty(_tracker$set, dimensions.WINDOW_ID, uuid()), _tracker$set));
  });

  // Adds tracking to record each the type, time, uuid, and visibility state
  // of each hit immediately before it's sent.
  ga(function (tracker) {
    var originalBuildHitTask = tracker.get('buildHitTask');
    tracker.set('buildHitTask', function (model) {
      var qt = model.get('queueTime') || 0;
      model.set(dimensions.HIT_TIME, String(new Date() - qt), true);
      model.set(dimensions.HIT_ID, uuid(), true);
      model.set(dimensions.HIT_TYPE, model.get('hitType'), true);
      model.set(dimensions.VISIBILITY_STATE, document.visibilityState, true);

      originalBuildHitTask(model);
    });
  });
};

/**
 * Gets the DOM and window load times and sends them as custom metrics to
 * Google Analytics via an event hit.
 */
var sendNavigationTimingMetrics = function sendNavigationTimingMetrics() {
  // Only track performance in supporting browsers.
  if (!(window.performance && window.performance.timing)) return;

  // If the window hasn't loaded, run this function after the `load` event.
  if (document.readyState != 'complete') {
    window.addEventListener('load', sendNavigationTimingMetrics);
    return;
  }

  var nt = performance.timing;
  var navStart = nt.navigationStart;

  var responseEnd = Math.round(nt.responseEnd - navStart);
  var domLoaded = Math.round(nt.domContentLoadedEventStart - navStart);
  var windowLoaded = Math.round(nt.loadEventStart - navStart);

  // In some edge cases browsers return very obviously incorrect NT values,
  // e.g. 0, negative, or future times. This validates values before sending.
  var allValuesAreValid = function allValuesAreValid() {
    for (var _len = arguments.length, values = Array(_len), _key = 0; _key < _len; _key++) {
      values[_key] = arguments[_key];
    }

    return values.every(function (value) {
      return value > 0 && value < 6e6;
    });
  };

  if (allValuesAreValid(responseEnd, domLoaded, windowLoaded)) {
    var _ga;

    ga('send', 'event', (_ga = {
      eventCategory: 'Navigation Timing',
      eventAction: 'track',
      eventLabel: NULL_VALUE,
      nonInteraction: true
    }, _defineProperty(_ga, metrics.RESPONSE_END_TIME, responseEnd), _defineProperty(_ga, metrics.DOM_LOAD_TIME, domLoaded), _defineProperty(_ga, metrics.WINDOW_LOAD_TIME, windowLoaded), _ga));
  }
};

/**
 * Accepts a custom dimension or metric and returns it's numerical index.
 * @param {string} definition The definition string (e.g. 'dimension1').
 * @return {number} The definition index.
 */
var getDefinitionIndex = function getDefinitionIndex(definition) {
  return +/\d+$/.exec(definition)[0];
};

/**
 * Generates a UUID.
 * https://gist.github.com/jed/982883
 * @param {string|undefined=} a
 * @return {string}
 */
var uuid = function b(a) {
  return a ? (a ^ Math.random() * 16 >> a / 4).toString(16) : ([1e7] + -1e3 + -4e3 + -8e3 + -1e11).replace(/[018]/g, b);
};

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
// Expose for Lift snippet, Analytics.scala

window.ga2 = { i: init };

/***/ }),

/***/ 186:
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_dom_utils__ = __webpack_require__(20);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_dom_utils___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_dom_utils__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1__constants__ = __webpack_require__(10);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2__method_chain__ = __webpack_require__(11);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3__provide__ = __webpack_require__(22);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4__usage__ = __webpack_require__(23);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_5__utilities__ = __webpack_require__(3);
var _slicedToArray = function () { function sliceIterator(arr, i) { var _arr = []; var _n = true; var _d = false; var _e = undefined; try { for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) { _arr.push(_s.value); if (i && _arr.length === i) break; } } catch (err) { _d = true; _e = err; } finally { try { if (!_n && _i["return"]) _i["return"](); } finally { if (_d) throw _e; } } return _arr; } return function (arr, i) { if (Array.isArray(arr)) { return arr; } else if (Symbol.iterator in Object(arr)) { return sliceIterator(arr, i); } else { throw new TypeError("Invalid attempt to destructure non-iterable instance"); } }; }();

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

/**
 * Copyright 2016 Google Inc. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */








/**
 * Class for the `cleanUrlTracker` analytics.js plugin.
 * @implements {CleanUrlTrackerPublicInterface}
 */

var CleanUrlTracker = function () {
  /**
   * Registers clean URL tracking on a tracker object. The clean URL tracker
   * removes query parameters from the page value reported to Google Analytics.
   * It also helps to prevent tracking similar URLs, e.g. sometimes ending a
   * URL with a slash and sometimes not.
   * @param {!Tracker} tracker Passed internally by analytics.js
   * @param {?CleanUrlTrackerOpts} opts Passed by the require command.
   */
  function CleanUrlTracker(tracker, opts) {
    _classCallCheck(this, CleanUrlTracker);

    Object(__WEBPACK_IMPORTED_MODULE_4__usage__["b" /* trackUsage */])(tracker, __WEBPACK_IMPORTED_MODULE_4__usage__["a" /* plugins */].CLEAN_URL_TRACKER);

    /** @type {CleanUrlTrackerOpts} */
    var defaultOpts = {
      // stripQuery: undefined,
      // queryParamsWhitelist: undefined,
      // queryDimensionIndex: undefined,
      // indexFilename: undefined,
      // trailingSlash: undefined,
      // urlFilter: undefined,
    };
    this.opts = /** @type {CleanUrlTrackerOpts} */Object(__WEBPACK_IMPORTED_MODULE_5__utilities__["a" /* assign */])(defaultOpts, opts);

    this.tracker = tracker;

    /** @type {string|null} */
    this.queryDimension = this.opts.stripQuery && this.opts.queryDimensionIndex ? 'dimension' + this.opts.queryDimensionIndex : null;

    // Binds methods to `this`.
    this.trackerGetOverride = this.trackerGetOverride.bind(this);
    this.buildHitTaskOverride = this.buildHitTaskOverride.bind(this);

    // Override built-in tracker method to watch for changes.
    __WEBPACK_IMPORTED_MODULE_2__method_chain__["a" /* default */].add(tracker, 'get', this.trackerGetOverride);
    __WEBPACK_IMPORTED_MODULE_2__method_chain__["a" /* default */].add(tracker, 'buildHitTask', this.buildHitTaskOverride);
  }

  /**
   * Ensures reads of the tracker object by other plugins always see the
   * "cleaned" versions of all URL fields.
   * @param {function(string):*} originalMethod A reference to the overridden
   *     method.
   * @return {function(string):*}
   */


  _createClass(CleanUrlTracker, [{
    key: 'trackerGetOverride',
    value: function trackerGetOverride(originalMethod) {
      var _this = this;

      return function (field) {
        if (field == 'page' || field == _this.queryDimension) {
          var fieldsObj = /** @type {!FieldsObj} */{
            location: originalMethod('location'),
            page: originalMethod('page')
          };
          var cleanedFieldsObj = _this.cleanUrlFields(fieldsObj);
          return cleanedFieldsObj[field];
        } else {
          return originalMethod(field);
        }
      };
    }

    /**
     * Cleans URL fields passed in a send command.
     * @param {function(!Model)} originalMethod A reference to the
     *     overridden method.
     * @return {function(!Model)}
     */

  }, {
    key: 'buildHitTaskOverride',
    value: function buildHitTaskOverride(originalMethod) {
      var _this2 = this;

      return function (model) {
        var cleanedFieldsObj = _this2.cleanUrlFields({
          location: model.get('location'),
          page: model.get('page')
        });
        model.set(cleanedFieldsObj, null, true);
        originalMethod(model);
      };
    }

    /**
     * Accepts of fields object containing URL fields and returns a new
     * fields object with the URLs "cleaned" according to the tracker options.
     * @param {!FieldsObj} fieldsObj
     * @return {!FieldsObj}
     */

  }, {
    key: 'cleanUrlFields',
    value: function cleanUrlFields(fieldsObj) {
      var url = Object(__WEBPACK_IMPORTED_MODULE_0_dom_utils__["parseUrl"])(
      /** @type {string} */fieldsObj.page || fieldsObj.location);

      var pathname = url.pathname;

      // If an index filename was provided, remove it if it appears at the end
      // of the URL.
      if (this.opts.indexFilename) {
        var parts = pathname.split('/');
        if (this.opts.indexFilename == parts[parts.length - 1]) {
          parts[parts.length - 1] = '';
          pathname = parts.join('/');
        }
      }

      // Ensure the URL ends with or doesn't end with slash based on the
      // `trailingSlash` option. Note that filename URLs should never contain
      // a trailing slash.
      if (this.opts.trailingSlash == 'remove') {
        pathname = pathname.replace(/\/+$/, '');
      } else if (this.opts.trailingSlash == 'add') {
        var isFilename = /\.\w+$/.test(pathname);
        if (!isFilename && pathname.substr(-1) != '/') {
          pathname = pathname + '/';
        }
      }

      /** @type {!FieldsObj} */
      var cleanedFieldsObj = {
        page: pathname + (this.opts.stripQuery ? this.stripNonWhitelistedQueryParams(url.search) : url.search)
      };
      if (fieldsObj.location) {
        cleanedFieldsObj.location = fieldsObj.location;
      }
      if (this.queryDimension) {
        cleanedFieldsObj[this.queryDimension] = url.search.slice(1) || __WEBPACK_IMPORTED_MODULE_1__constants__["b" /* NULL_DIMENSION */];
      }

      // Apply the `urlFieldsFilter()` option if passed.
      if (typeof this.opts.urlFieldsFilter == 'function') {
        /** @type {!FieldsObj} */
        var userCleanedFieldsObj = this.opts.urlFieldsFilter(cleanedFieldsObj, __WEBPACK_IMPORTED_MODULE_0_dom_utils__["parseUrl"]);

        // Ensure only the URL fields are returned.
        var returnValue = {
          page: userCleanedFieldsObj.page,
          location: userCleanedFieldsObj.location
        };
        if (this.queryDimension) {
          returnValue[this.queryDimension] = userCleanedFieldsObj[this.queryDimension];
        }
        return returnValue;
      } else {
        return cleanedFieldsObj;
      }
    }

    /**
     * Accpets a raw URL search string and returns a new search string containing
     * only the site search params (if they exist).
     * @param {string} searchString The URL search string (starting with '?').
     * @return {string} The query string
     */

  }, {
    key: 'stripNonWhitelistedQueryParams',
    value: function stripNonWhitelistedQueryParams(searchString) {
      var _this3 = this;

      if (Array.isArray(this.opts.queryParamsWhitelist)) {
        var foundParams = [];
        searchString.slice(1).split('&').forEach(function (kv) {
          var _kv$split = kv.split('='),
              _kv$split2 = _slicedToArray(_kv$split, 2),
              key = _kv$split2[0],
              value = _kv$split2[1];

          if (_this3.opts.queryParamsWhitelist.indexOf(key) > -1 && value) {
            foundParams.push([key, value]);
          }
        });

        return foundParams.length ? '?' + foundParams.map(function (kv) {
          return kv.join('=');
        }).join('&') : '';
      } else {
        return '';
      }
    }

    /**
     * Restores all overridden tasks and methods.
     */

  }, {
    key: 'remove',
    value: function remove() {
      __WEBPACK_IMPORTED_MODULE_2__method_chain__["a" /* default */].remove(this.tracker, 'get', this.trackerGetOverride);
      __WEBPACK_IMPORTED_MODULE_2__method_chain__["a" /* default */].remove(this.tracker, 'buildHitTask', this.buildHitTaskOverride);
    }
  }]);

  return CleanUrlTracker;
}();

Object(__WEBPACK_IMPORTED_MODULE_3__provide__["a" /* default */])('cleanUrlTracker', CleanUrlTracker);

/***/ }),

/***/ 187:
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = delegate;

var _closest = __webpack_require__(146);

var _closest2 = _interopRequireDefault(_closest);

var _matches = __webpack_require__(21);

var _matches2 = _interopRequireDefault(_matches);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

/**
 * Delegates the handling of events for an element matching a selector to an
 * ancestor of the matching element.
 * @param {!Node} ancestor The ancestor element to add the listener to.
 * @param {string} eventType The event type to listen to.
 * @param {string} selector A CSS selector to match against child elements.
 * @param {!Function} callback A function to run any time the event happens.
 * @param {Object=} opts A configuration options object. The available options:
 *     - useCapture<boolean>: If true, bind to the event capture phase.
 *     - deep<boolean>: If true, delegate into shadow trees.
 * @return {Object} The delegate object. It contains a destroy method.
 */
function delegate(ancestor, eventType, selector, callback) {
  var opts = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : {};

  // Defines the event listener.
  var listener = function listener(event) {
    var delegateTarget = void 0;

    // If opts.composed is true and the event originated from inside a Shadow
    // tree, check the composed path nodes.
    if (opts.composed && typeof event.composedPath == 'function') {
      var composedPath = event.composedPath();
      for (var i = 0, node; node = composedPath[i]; i++) {
        if (node.nodeType == 1 && (0, _matches2.default)(node, selector)) {
          delegateTarget = node;
        }
      }
    } else {
      // Otherwise check the parents.
      delegateTarget = (0, _closest2.default)(event.target, selector, true);
    }

    if (delegateTarget) {
      callback.call(delegateTarget, event, delegateTarget);
    }
  };

  ancestor.addEventListener(eventType, listener, opts.useCapture);

  return {
    destroy: function destroy() {
      ancestor.removeEventListener(eventType, listener, opts.useCapture);
    }
  };
}

/***/ }),

/***/ 188:
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});

var _typeof = typeof Symbol === "function" && typeof Symbol.iterator === "symbol" ? function (obj) { return typeof obj; } : function (obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; };

exports.default = dispatch;
/**
 * Dispatches an event on the passed element.
 * @param {!Element} element The DOM element to dispatch the event on.
 * @param {string} eventType The type of event to dispatch.
 * @param {Object|string=} eventName A string name of the event constructor
 *     to use. Defaults to 'Event' if nothing is passed or 'CustomEvent' if
 *     a value is set on `initDict.detail`. If eventName is given an object
 *     it is assumed to be initDict and thus reassigned.
 * @param {Object=} initDict The initialization attributes for the
 *     event. A `detail` property can be used here to pass custom data.
 * @return {boolean} The return value of `element.dispatchEvent`, which will
 *     be false if any of the event listeners called `preventDefault`.
 */
function dispatch(element, eventType) {
  var eventName = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 'Event';
  var initDict = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : {};

  var event = void 0;
  var isCustom = void 0;

  // eventName is optional
  if ((typeof eventName === 'undefined' ? 'undefined' : _typeof(eventName)) == 'object') {
    initDict = eventName;
    eventName = 'Event';
  }

  initDict.bubbles = initDict.bubbles || false;
  initDict.cancelable = initDict.cancelable || false;
  initDict.composed = initDict.composed || false;

  // If a detail property is passed, this is a custom event.
  if ('detail' in initDict) isCustom = true;
  eventName = isCustom ? 'CustomEvent' : eventName;

  // Tries to create the event using constructors, if that doesn't work,
  // fallback to `document.createEvent()`.
  try {
    event = new window[eventName](eventType, initDict);
  } catch (err) {
    event = document.createEvent(eventName);
    var initMethod = 'init' + (isCustom ? 'Custom' : '') + 'Event';
    event[initMethod](eventType, initDict.bubbles, initDict.cancelable, initDict.detail);
  }

  return element.dispatchEvent(event);
}

/***/ }),

/***/ 189:
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = getAttributes;
/**
 * Gets all attributes of an element as a plain JavaScriot object.
 * @param {Element} element The element whose attributes to get.
 * @return {!Object} An object whose keys are the attribute keys and whose
 *     values are the attribute values. If no attributes exist, an empty
 *     object is returned.
 */
function getAttributes(element) {
  var attrs = {};

  // Validate input.
  if (!(element && element.nodeType == 1)) return attrs;

  // Return an empty object if there are no attributes.
  var map = element.attributes;
  if (map.length === 0) return {};

  for (var i = 0, attr; attr = map[i]; i++) {
    attrs[attr.name] = attr.value;
  }
  return attrs;
}

/***/ }),

/***/ 190:
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = parseUrl;
var HTTP_PORT = '80';
var HTTPS_PORT = '443';
var DEFAULT_PORT = RegExp(':(' + HTTP_PORT + '|' + HTTPS_PORT + ')$');

var a = document.createElement('a');
var cache = {};

/**
 * Parses the given url and returns an object mimicing a `Location` object.
 * @param {string} url The url to parse.
 * @return {!Object} An object with the same properties as a `Location`.
 */
function parseUrl(url) {
  // All falsy values (as well as ".") should map to the current URL.
  url = !url || url == '.' ? location.href : url;

  if (cache[url]) return cache[url];

  a.href = url;

  // When parsing file relative paths (e.g. `../index.html`), IE will correctly
  // resolve the `href` property but will keep the `..` in the `path` property.
  // It will also not include the `host` or `hostname` properties. Furthermore,
  // IE will sometimes return no protocol or just a colon, especially for things
  // like relative protocol URLs (e.g. "//google.com").
  // To workaround all of these issues, we reparse with the full URL from the
  // `href` property.
  if (url.charAt(0) == '.' || url.charAt(0) == '/') return parseUrl(a.href);

  // Don't include default ports.
  var port = a.port == HTTP_PORT || a.port == HTTPS_PORT ? '' : a.port;

  // PhantomJS sets the port to "0" when using the file: protocol.
  port = port == '0' ? '' : port;

  // Sometimes IE incorrectly includes a port for default ports
  // (e.g. `:80` or `:443`) even when no port is specified in the URL.
  // http://bit.ly/1rQNoMg
  var host = a.host.replace(DEFAULT_PORT, '');

  // Not all browser support `origin` so we have to build it.
  var origin = a.origin ? a.origin : a.protocol + '//' + host;

  // Sometimes IE doesn't include the leading slash for pathname.
  // http://bit.ly/1rQNoMg
  var pathname = a.pathname.charAt(0) == '/' ? a.pathname : '/' + a.pathname;

  return cache[url] = {
    hash: a.hash,
    host: host,
    hostname: a.hostname,
    href: a.href,
    origin: origin,
    pathname: pathname,
    port: port,
    protocol: a.protocol,
    search: a.search
  };
}

/***/ }),

/***/ 191:
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_dom_utils__ = __webpack_require__(20);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_dom_utils___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_dom_utils__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1__provide__ = __webpack_require__(22);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2__usage__ = __webpack_require__(23);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3__utilities__ = __webpack_require__(3);
var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

/**
 * Copyright 2016 Google Inc. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */






/**
 * Class for the `outboundLinkTracker` analytics.js plugin.
 * @implements {OutboundLinkTrackerPublicInterface}
 */

var OutboundLinkTracker = function () {
  /**
   * Registers outbound link tracking on a tracker object.
   * @param {!Tracker} tracker Passed internally by analytics.js
   * @param {?Object} opts Passed by the require command.
   */
  function OutboundLinkTracker(tracker, opts) {
    var _this = this;

    _classCallCheck(this, OutboundLinkTracker);

    Object(__WEBPACK_IMPORTED_MODULE_2__usage__["b" /* trackUsage */])(tracker, __WEBPACK_IMPORTED_MODULE_2__usage__["a" /* plugins */].OUTBOUND_LINK_TRACKER);

    // Feature detects to prevent errors in unsupporting browsers.
    if (!window.addEventListener) return;

    /** @type {OutboundLinkTrackerOpts} */
    var defaultOpts = {
      events: ['click'],
      linkSelector: 'a, area',
      shouldTrackOutboundLink: this.shouldTrackOutboundLink,
      fieldsObj: {},
      attributePrefix: 'ga-'
      // hitFilter: undefined,
    };

    this.opts = /** @type {OutboundLinkTrackerOpts} */Object(__WEBPACK_IMPORTED_MODULE_3__utilities__["a" /* assign */])(defaultOpts, opts);

    this.tracker = tracker;

    // Binds methods.
    this.handleLinkInteractions = this.handleLinkInteractions.bind(this);

    // Creates a mapping of events to their delegates
    this.delegates = {};
    this.opts.events.forEach(function (event) {
      _this.delegates[event] = Object(__WEBPACK_IMPORTED_MODULE_0_dom_utils__["delegate"])(document, event, _this.opts.linkSelector, _this.handleLinkInteractions, { composed: true, useCapture: true });
    });
  }

  /**
   * Handles all interactions on link elements. A link is considered an outbound
   * link if its hostname property does not match location.hostname. When the
   * beacon transport method is not available, the links target is set to
   * "_blank" to ensure the hit can be sent.
   * @param {Event} event The DOM click event.
   * @param {Element} link The delegated event target.
   */


  _createClass(OutboundLinkTracker, [{
    key: 'handleLinkInteractions',
    value: function handleLinkInteractions(event, link) {
      var _this2 = this;

      if (this.opts.shouldTrackOutboundLink(link, __WEBPACK_IMPORTED_MODULE_0_dom_utils__["parseUrl"])) {
        var href = link.getAttribute('href') || link.getAttribute('xlink:href');
        var url = Object(__WEBPACK_IMPORTED_MODULE_0_dom_utils__["parseUrl"])(href);

        /** @type {FieldsObj} */
        var defaultFields = {
          transport: 'beacon',
          eventCategory: 'Outbound Link',
          eventAction: event.type,
          eventLabel: url.href
        };

        /** @type {FieldsObj} */
        var userFields = Object(__WEBPACK_IMPORTED_MODULE_3__utilities__["a" /* assign */])({}, this.opts.fieldsObj, Object(__WEBPACK_IMPORTED_MODULE_3__utilities__["e" /* getAttributeFields */])(link, this.opts.attributePrefix));

        var fieldsObj = Object(__WEBPACK_IMPORTED_MODULE_3__utilities__["c" /* createFieldsObj */])(defaultFields, userFields, this.tracker, this.opts.hitFilter, link, event);

        if (!navigator.sendBeacon && linkClickWillUnloadCurrentPage(event, link)) {
          // Adds a new event handler at the last minute to minimize the chances
          // that another event handler for this click will run after this logic.
          var clickHandler = function clickHandler() {
            window.removeEventListener('click', clickHandler);

            // Checks to make sure another event handler hasn't already prevented
            // the default action. If it has the custom redirect isn't needed.
            if (!event.defaultPrevented) {
              // Stops the click and waits until the hit is complete (with
              // timeout) for browsers that don't support beacon.
              event.preventDefault();

              var oldHitCallback = fieldsObj.hitCallback;
              fieldsObj.hitCallback = Object(__WEBPACK_IMPORTED_MODULE_3__utilities__["i" /* withTimeout */])(function () {
                if (typeof oldHitCallback == 'function') oldHitCallback();
                location.href = href;
              });
            }
            _this2.tracker.send('event', fieldsObj);
          };
          window.addEventListener('click', clickHandler);
        } else {
          this.tracker.send('event', fieldsObj);
        }
      }
    }

    /**
     * Determines whether or not the tracker should send a hit when a link is
     * clicked. By default links with a hostname property not equal to the current
     * hostname are tracked.
     * @param {Element} link The link that was clicked on.
     * @param {Function} parseUrlFn A cross-browser utility method for url
     *     parsing (note: renamed to disambiguate when compiling).
     * @return {boolean} Whether or not the link should be tracked.
     */

  }, {
    key: 'shouldTrackOutboundLink',
    value: function shouldTrackOutboundLink(link, parseUrlFn) {
      var href = link.getAttribute('href') || link.getAttribute('xlink:href');
      var url = parseUrlFn(href);
      return url.hostname != location.hostname && url.protocol.slice(0, 4) == 'http';
    }

    /**
     * Removes all event listeners and instance properties.
     */

  }, {
    key: 'remove',
    value: function remove() {
      var _this3 = this;

      Object.keys(this.delegates).forEach(function (key) {
        _this3.delegates[key].destroy();
      });
    }
  }]);

  return OutboundLinkTracker;
}();

Object(__WEBPACK_IMPORTED_MODULE_1__provide__["a" /* default */])('outboundLinkTracker', OutboundLinkTracker);

/**
 * Determines if a link click event will cause the current page to upload.
 * Note: most link clicks *will* cause the current page to unload because they
 * initiate a page navigation. The most common reason a link click won't cause
 * the page to unload is if the clicked was to open the link in a new tab.
 * @param {Event} event The DOM event.
 * @param {Element} link The link element clicked on.
 * @return {boolean} True if the current page will be unloaded.
 */
function linkClickWillUnloadCurrentPage(event, link) {
  return !(
  // The event type can be customized; we only care about clicks here.
  event.type != 'click' ||
  // Links with target="_blank" set will open in a new window/tab.
  link.target == '_blank' ||
  // On mac, command clicking will open a link in a new tab. Control
  // clicking does this on windows.
  event.metaKey || event.ctrlKey ||
  // Shift clicking in Chrome/Firefox opens the link in a new window
  // In Safari it adds the URL to a favorites list.
  event.shiftKey ||
  // On Mac, clicking with the option key is used to download a resouce.
  event.altKey ||
  // Middle mouse button clicks (which == 2) are used to open a link
  // in a new tab, and right clicks (which == 3) on Firefox trigger
  // a click event.
  event.which > 1);
}

/***/ }),

/***/ 192:
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0__constants__ = __webpack_require__(10);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1__method_chain__ = __webpack_require__(11);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2__provide__ = __webpack_require__(22);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_3__session__ = __webpack_require__(193);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_4__store__ = __webpack_require__(148);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_5__usage__ = __webpack_require__(23);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_6__utilities__ = __webpack_require__(3);
var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

/**
 * Copyright 2016 Google Inc. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */









var HIDDEN = 'hidden';
var VISIBLE = 'visible';
var PAGE_ID = Object(__WEBPACK_IMPORTED_MODULE_6__utilities__["h" /* uuid */])();
var SECONDS = 1000;

/**
 * Class for the `pageVisibilityTracker` analytics.js plugin.
 * @implements {PageVisibilityTrackerPublicInterface}
 */

var PageVisibilityTracker = function () {
  /**
   * Registers outbound link tracking on tracker object.
   * @param {!Tracker} tracker Passed internally by analytics.js
   * @param {?Object} opts Passed by the require command.
   */
  function PageVisibilityTracker(tracker, opts) {
    var _this = this;

    _classCallCheck(this, PageVisibilityTracker);

    Object(__WEBPACK_IMPORTED_MODULE_5__usage__["b" /* trackUsage */])(tracker, __WEBPACK_IMPORTED_MODULE_5__usage__["a" /* plugins */].PAGE_VISIBILITY_TRACKER);

    // Feature detects to prevent errors in unsupporting browsers.
    if (!document.visibilityState) return;

    /** @type {PageVisibilityTrackerOpts} */
    var defaultOpts = {
      sessionTimeout: __WEBPACK_IMPORTED_MODULE_3__session__["a" /* default */].DEFAULT_TIMEOUT,
      visibleThreshold: 5 * SECONDS,
      // timeZone: undefined,
      sendInitialPageview: false,
      // pageLoadsMetricIndex: undefined,
      // visibleMetricIndex: undefined,
      fieldsObj: {}
      // hitFilter: undefined
    };

    this.opts = /** @type {PageVisibilityTrackerOpts} */Object(__WEBPACK_IMPORTED_MODULE_6__utilities__["a" /* assign */])(defaultOpts, opts);

    this.tracker = tracker;
    this.lastPageState = document.visibilityState;
    this.visibleThresholdTimeout_ = null;
    this.isInitialPageviewSent_ = false;

    // Binds methods to `this`.
    this.trackerSetOverride = this.trackerSetOverride.bind(this);
    this.handleChange = this.handleChange.bind(this);
    this.handleWindowUnload = this.handleWindowUnload.bind(this);
    this.handleExternalStoreSet = this.handleExternalStoreSet.bind(this);

    // Creates the store and binds storage change events.
    this.store = __WEBPACK_IMPORTED_MODULE_4__store__["a" /* default */].getOrCreate(tracker.get('trackingId'), 'plugins/page-visibility-tracker');
    this.store.on('externalSet', this.handleExternalStoreSet);

    // Creates the session and binds session events.
    this.session = __WEBPACK_IMPORTED_MODULE_3__session__["a" /* default */].getOrCreate(tracker, this.opts.sessionTimeout, this.opts.timeZone);

    // Override the built-in tracker.set method to watch for changes.
    __WEBPACK_IMPORTED_MODULE_1__method_chain__["a" /* default */].add(tracker, 'set', this.trackerSetOverride);

    window.addEventListener('unload', this.handleWindowUnload);
    document.addEventListener('visibilitychange', this.handleChange);

    // Postpone sending any hits until the next call stack, which allows all
    // autotrack plugins to be required sync before any hits are sent.
    Object(__WEBPACK_IMPORTED_MODULE_6__utilities__["d" /* deferUntilPluginsLoaded */])(this.tracker, function () {
      if (document.visibilityState == VISIBLE) {
        if (_this.opts.sendInitialPageview) {
          _this.sendPageview({ isPageLoad: true });
          _this.isInitialPageviewSent_ = true;
        }
        _this.store.set( /** @type {PageVisibilityStoreData} */{
          time: Object(__WEBPACK_IMPORTED_MODULE_6__utilities__["g" /* now */])(),
          state: VISIBLE,
          pageId: PAGE_ID,
          sessionId: _this.session.getId()
        });
      } else {
        if (_this.opts.sendInitialPageview && _this.opts.pageLoadsMetricIndex) {
          _this.sendPageLoad();
        }
      }
    });
  }

  /**
   * Inspects the last visibility state change data and determines if a
   * visibility event needs to be tracked based on the current visibility
   * state and whether or not the session has expired. If the session has
   * expired, a change to `visible` will trigger an additional pageview.
   * This method also sends as the event value (and optionally a custom metric)
   * the elapsed time between this event and the previously reported change
   * in the same session, allowing you to more accurately determine when users
   * were actually looking at your page versus when it was in the background.
   */


  _createClass(PageVisibilityTracker, [{
    key: 'handleChange',
    value: function handleChange() {
      var _this2 = this;

      if (!(document.visibilityState == VISIBLE || document.visibilityState == HIDDEN)) {
        return;
      }

      var lastStoredChange = this.getAndValidateChangeData();

      /** @type {PageVisibilityStoreData} */
      var change = {
        time: Object(__WEBPACK_IMPORTED_MODULE_6__utilities__["g" /* now */])(),
        state: document.visibilityState,
        pageId: PAGE_ID,
        sessionId: this.session.getId()
      };

      // If the visibilityState has changed to visible and the initial pageview
      // has not been sent (and the `sendInitialPageview` option is `true`).
      // Send the initial pageview now.
      if (document.visibilityState == VISIBLE && this.opts.sendInitialPageview && !this.isInitialPageviewSent_) {
        this.sendPageview();
        this.isInitialPageviewSent_ = true;
      }

      // If the visibilityState has changed to hidden, clear any scheduled
      // pageviews waiting for the visibleThreshold timeout.
      if (document.visibilityState == HIDDEN && this.visibleThresholdTimeout_) {
        clearTimeout(this.visibleThresholdTimeout_);
      }

      if (this.session.isExpired(lastStoredChange.sessionId)) {
        this.store.clear();
        if (this.lastPageState == HIDDEN && document.visibilityState == VISIBLE) {
          // If the session has expired, changes from hidden to visible should
          // be considered a new pageview rather than a visibility event.
          // This behavior ensures all sessions contain a pageview so
          // session-level page dimensions and metrics (e.g. ga:landingPagePath
          // and ga:entrances) are correct.
          // Also, in order to prevent false positives, we add a small timeout
          // that is cleared if the visibilityState changes to hidden shortly
          // after the change to visible. This can happen if a user is quickly
          // switching through their open tabs but not actually interacting with
          // and of them. It can also happen when a user goes to a tab just to
          // immediately close it. Such cases should not be considered pageviews.
          clearTimeout(this.visibleThresholdTimeout_);
          this.visibleThresholdTimeout_ = setTimeout(function () {
            _this2.store.set(change);
            _this2.sendPageview({ hitTime: change.time });
          }, this.opts.visibleThreshold);
        }
      } else {
        if (lastStoredChange.pageId == PAGE_ID && lastStoredChange.state == VISIBLE) {
          this.sendPageVisibilityEvent(lastStoredChange);
        }
        this.store.set(change);
      }

      this.lastPageState = document.visibilityState;
    }

    /**
     * Retroactively updates the stored change data in cases where it's known to
     * be out of sync.
     * This plugin keeps track of each visiblity change and stores the last one
     * in localStorage. LocalStorage is used to handle situations where the user
     * has multiple page open at the same time and we don't want to
     * double-report page visibility in those cases.
     * However, a problem can occur if a user closes a page when one or more
     * visible pages are still open. In such cases it's impossible to know
     * which of the remaining pages the user will interact with next.
     * To solve this problem we wait for the next change on any page and then
     * retroactively update the stored data to reflect the current page as being
     * the page on which the last change event occured and measure visibility
     * from that point.
     * @return {!PageVisibilityStoreData}
     */

  }, {
    key: 'getAndValidateChangeData',
    value: function getAndValidateChangeData() {
      var lastStoredChange =
      /** @type {PageVisibilityStoreData} */this.store.get();

      if (this.lastPageState == VISIBLE && lastStoredChange.state == HIDDEN && lastStoredChange.pageId != PAGE_ID) {
        lastStoredChange.state = VISIBLE;
        lastStoredChange.pageId = PAGE_ID;
        this.store.set(lastStoredChange);
      }
      return lastStoredChange;
    }

    /**
     * Sends a Page Visibility event to track the time this page was in the
     * visible state (assuming it was in that state long enough to meet the
     * threshold).
     * @param {!PageVisibilityStoreData} lastStoredChange
     * @param {{hitTime: (number|undefined)}=} param1
     *     - hitTime: A hit timestap used to help ensure original order in cases
     *                where the send is delayed.
     */

  }, {
    key: 'sendPageVisibilityEvent',
    value: function sendPageVisibilityEvent(lastStoredChange) {
      var _ref = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {},
          hitTime = _ref.hitTime;

      var delta = this.getTimeSinceLastStoredChange(lastStoredChange, { hitTime: hitTime });

      // If the detla is greater than the visibileThreshold, report it.
      if (delta && delta >= this.opts.visibleThreshold) {
        var deltaInSeconds = Math.round(delta / SECONDS);

        /** @type {FieldsObj} */
        var defaultFields = {
          transport: 'beacon',
          nonInteraction: true,
          eventCategory: 'Page Visibility',
          eventAction: 'track',
          eventValue: deltaInSeconds,
          eventLabel: __WEBPACK_IMPORTED_MODULE_0__constants__["b" /* NULL_DIMENSION */]
        };

        if (hitTime) {
          defaultFields.queueTime = Object(__WEBPACK_IMPORTED_MODULE_6__utilities__["g" /* now */])() - hitTime;
        }

        // If a custom metric was specified, set it equal to the event value.
        if (this.opts.visibleMetricIndex) {
          defaultFields['metric' + this.opts.visibleMetricIndex] = deltaInSeconds;
        }

        this.tracker.send('event', Object(__WEBPACK_IMPORTED_MODULE_6__utilities__["c" /* createFieldsObj */])(defaultFields, this.opts.fieldsObj, this.tracker, this.opts.hitFilter));
      }
    }

    /**
     * Sends a page load event.
     */

  }, {
    key: 'sendPageLoad',
    value: function sendPageLoad() {
      var _defaultFields;

      /** @type {FieldsObj} */
      var defaultFields = (_defaultFields = {
        transport: 'beacon',
        eventCategory: 'Page Visibility',
        eventAction: 'page load',
        eventLabel: __WEBPACK_IMPORTED_MODULE_0__constants__["b" /* NULL_DIMENSION */]
      }, _defineProperty(_defaultFields, 'metric' + this.opts.pageLoadsMetricIndex, 1), _defineProperty(_defaultFields, 'nonInteraction', true), _defaultFields);
      this.tracker.send('event', Object(__WEBPACK_IMPORTED_MODULE_6__utilities__["c" /* createFieldsObj */])(defaultFields, this.opts.fieldsObj, this.tracker, this.opts.hitFilter));
    }

    /**
     * Sends a pageview, optionally calculating an offset if hitTime is passed.
     * @param {{
     *   hitTime: (number|undefined),
     *   isPageLoad: (boolean|undefined)
     * }=} param1
     *     hitTime: The timestamp of the current hit.
     *     isPageLoad: True if this pageview was also a page load.
     */

  }, {
    key: 'sendPageview',
    value: function sendPageview() {
      var _ref2 = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {},
          hitTime = _ref2.hitTime,
          isPageLoad = _ref2.isPageLoad;

      /** @type {FieldsObj} */
      var defaultFields = { transport: 'beacon' };
      if (hitTime) {
        defaultFields.queueTime = Object(__WEBPACK_IMPORTED_MODULE_6__utilities__["g" /* now */])() - hitTime;
      }
      if (isPageLoad && this.opts.pageLoadsMetricIndex) {
        defaultFields['metric' + this.opts.pageLoadsMetricIndex] = 1;
      }

      this.tracker.send('pageview', Object(__WEBPACK_IMPORTED_MODULE_6__utilities__["c" /* createFieldsObj */])(defaultFields, this.opts.fieldsObj, this.tracker, this.opts.hitFilter));
    }

    /**
     * Detects changes to the tracker object and triggers an update if the page
     * field has changed.
     * @param {function((Object|string), (string|undefined))} originalMethod
     *     A reference to the overridden method.
     * @return {function((Object|string), (string|undefined))}
     */

  }, {
    key: 'trackerSetOverride',
    value: function trackerSetOverride(originalMethod) {
      var _this3 = this;

      return function (field, value) {
        /** @type {!FieldsObj} */
        var fields = Object(__WEBPACK_IMPORTED_MODULE_6__utilities__["f" /* isObject */])(field) ? field : _defineProperty({}, field, value);
        if (fields.page && fields.page !== _this3.tracker.get('page')) {
          if (_this3.lastPageState == VISIBLE) {
            _this3.handleChange();
          }
        }
        originalMethod(field, value);
      };
    }

    /**
     * Calculates the time since the last visibility change event in the current
     * session. If the session has expired the reported time is zero.
     * @param {PageVisibilityStoreData} lastStoredChange
     * @param {{hitTime: (number|undefined)}=} param1
     *     hitTime: The time of the current hit (defaults to now).
     * @return {number} The time (in ms) since the last change.
     */

  }, {
    key: 'getTimeSinceLastStoredChange',
    value: function getTimeSinceLastStoredChange(lastStoredChange) {
      var _ref4 = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : {},
          hitTime = _ref4.hitTime;

      return lastStoredChange.time ? (hitTime || Object(__WEBPACK_IMPORTED_MODULE_6__utilities__["g" /* now */])()) - lastStoredChange.time : 0;
    }

    /**
     * Handles responding to the `storage` event.
     * The code on this page needs to be informed when other tabs or windows are
     * updating the stored page visibility state data. This method checks to see
     * if a hidden state is stored when there are still visible tabs open, which
     * can happen if multiple windows are open at the same time.
     * @param {PageVisibilityStoreData} newData
     * @param {PageVisibilityStoreData} oldData
     */

  }, {
    key: 'handleExternalStoreSet',
    value: function handleExternalStoreSet(newData, oldData) {
      // If the change times are the same, then the previous write only
      // updated the active page ID. It didn't enter a new state and thus no
      // hits should be sent.
      if (newData.time == oldData.time) return;

      // Page Visibility events must be sent by the tracker on the page
      // where the original event occurred. So if a change happens on another
      // page, but this page is where the previous change event occurred, then
      // this page is the one that needs to send the event (so all dimension
      // data is correct).
      if (oldData.pageId == PAGE_ID && oldData.state == VISIBLE && !this.session.isExpired(oldData.sessionId)) {
        this.sendPageVisibilityEvent(oldData, { hitTime: newData.time });
      }
    }

    /**
     * Handles responding to the `unload` event.
     * Since some browsers don't emit a `visibilitychange` event in all cases
     * where a page might be unloaded, it's necessary to hook into the `unload`
     * event to ensure the correct state is always stored.
     */

  }, {
    key: 'handleWindowUnload',
    value: function handleWindowUnload() {
      // If the stored visibility state isn't hidden when the unload event
      // fires, it means the visibilitychange event didn't fire as the document
      // was being unloaded, so we invoke it manually.
      if (this.lastPageState != HIDDEN) {
        this.handleChange();
      }
    }

    /**
     * Removes all event listeners and restores overridden methods.
     */

  }, {
    key: 'remove',
    value: function remove() {
      this.store.destroy();
      this.session.destroy();
      __WEBPACK_IMPORTED_MODULE_1__method_chain__["a" /* default */].remove(this.tracker, 'set', this.trackerSetOverride);
      window.removeEventListener('unload', this.handleWindowUnload);
      document.removeEventListener('visibilitychange', this.handleChange);
    }
  }]);

  return PageVisibilityTracker;
}();

Object(__WEBPACK_IMPORTED_MODULE_2__provide__["a" /* default */])('pageVisibilityTracker', PageVisibilityTracker);

/***/ }),

/***/ 193:
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0__method_chain__ = __webpack_require__(11);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1__store__ = __webpack_require__(148);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_2__utilities__ = __webpack_require__(3);
var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

/**
 * Copyright 2016 Google Inc. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */





var SECONDS = 1000;
var MINUTES = 60 * SECONDS;

var instances = {};

/**
 * A session management class that helps track session boundaries
 * across multiple open tabs/windows.
 */

var Session = function () {
  _createClass(Session, null, [{
    key: 'getOrCreate',

    /**
     * Gets an existing instance for the passed arguments or creates a new
     * instance if one doesn't exist.
     * @param {!Tracker} tracker An analytics.js tracker object.
     * @param {number} timeout The session timeout (in minutes). This value
     *     should match what's set in the "Session settings" section of the
     *     Google Analytics admin.
     * @param {string=} timeZone The optional IANA time zone of the view. This
     *     value should match what's set in the "View settings" section of the
     *     Google Analytics admin. (Note: this assumes all views for the property
     *     use the same time zone. If that's not true, it's better not to use
     *     this feature).
     * @return {Session} The Session instance.
     */
    value: function getOrCreate(tracker, timeout, timeZone) {
      // Don't create multiple instances for the same property.
      var trackingId = tracker.get('trackingId');
      if (instances[trackingId]) {
        return instances[trackingId];
      } else {
        return instances[trackingId] = new Session(tracker, timeout, timeZone);
      }
    }

    /**
     * @param {!Tracker} tracker An analytics.js tracker object.
     * @param {number} timeout The session timeout (in minutes). This value
     *     should match what's set in the "Session settings" section of the
     *     Google Analytics admin.
     * @param {string=} timeZone The optional IANA time zone of the view. This
     *     value should match what's set in the "View settings" section of the
     *     Google Analytics admin. (Note: this assumes all views for the property
     *     use the same time zone. If that's not true, it's better not to use
     *     this feature).
     */

  }]);

  function Session(tracker, timeout, timeZone) {
    _classCallCheck(this, Session);

    this.tracker = tracker;
    this.timeout = timeout || Session.DEFAULT_TIMEOUT;
    this.timeZone = timeZone;

    // Binds methods.
    this.sendHitTaskOverride = this.sendHitTaskOverride.bind(this);

    // Overrides into the trackers sendHitTask method.
    __WEBPACK_IMPORTED_MODULE_0__method_chain__["a" /* default */].add(tracker, 'sendHitTask', this.sendHitTaskOverride);

    // Some browser doesn't support various features of the
    // `Intl.DateTimeFormat` API, so we have to try/catch it. Consequently,
    // this allows us to assume the presence of `this.dateTimeFormatter` means
    // it works in the current browser.
    try {
      this.dateTimeFormatter = new Intl.DateTimeFormat('en-US', { timeZone: this.timeZone });
    } catch (err) {}
    // Do nothing.


    /** @type {SessionStoreData} */
    var defaultProps = {
      hitTime: 0,
      isExpired: false
    };
    this.store = __WEBPACK_IMPORTED_MODULE_1__store__["a" /* default */].getOrCreate(tracker.get('trackingId'), 'session', defaultProps);

    // Ensure the session has an ID.
    if (!this.store.get().id) {
      this.store.set( /** @type {SessionStoreData} */{ id: Object(__WEBPACK_IMPORTED_MODULE_2__utilities__["h" /* uuid */])() });
    }
  }

  /**
   * Returns the ID of the current session.
   * @return {string}
   */


  _createClass(Session, [{
    key: 'getId',
    value: function getId() {
      return this.store.get().id;
    }

    /**
     * Accepts a session ID and returns true if the specified session has
     * evidentially expired. A session can expire for two reasons:
     *   - More than 30 minutes has elapsed since the previous hit
     *     was sent (The 30 minutes number is the Google Analytics default, but
     *     it can be modified in GA admin "Session settings").
     *   - A new day has started since the previous hit, in the
     *     specified time zone (should correspond to the time zone of the
     *     property's views).
     *
     * Note: since real session boundaries are determined at processing time,
     * this is just a best guess rather than a source of truth.
     *
     * @param {string} id The ID of a session to check for expiry.
     * @return {boolean} True if the session has not exp
     */

  }, {
    key: 'isExpired',
    value: function isExpired() {
      var id = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : this.getId();

      // If a session ID is passed and it doesn't match the current ID,
      // assume it's from an expired session. If no ID is passed, assume the ID
      // of the current session.
      if (id != this.getId()) return true;

      /** @type {SessionStoreData} */
      var sessionData = this.store.get();

      // `isExpired` will be `true` if the sessionControl field was set to
      // 'end' on the previous hit.
      if (sessionData.isExpired) return true;

      var oldHitTime = sessionData.hitTime;

      // Only consider a session expired if previous hit time data exists, and
      // the previous hit time is greater than that session timeout period or
      // the hits occurred on different days in the session timezone.
      if (oldHitTime) {
        var currentDate = new Date();
        var oldHitDate = new Date(oldHitTime);
        if (currentDate - oldHitDate > this.timeout * MINUTES || this.datesAreDifferentInTimezone(currentDate, oldHitDate)) {
          return true;
        }
      }

      // For all other cases return false.
      return false;
    }

    /**
     * Returns true if (and only if) the timezone date formatting is supported
     * in the current browser and if the two dates are definitively not the
     * same date in the session timezone. Anything short of this returns false.
     * @param {!Date} d1
     * @param {!Date} d2
     * @return {boolean}
     */

  }, {
    key: 'datesAreDifferentInTimezone',
    value: function datesAreDifferentInTimezone(d1, d2) {
      if (!this.dateTimeFormatter) {
        return false;
      } else {
        return this.dateTimeFormatter.format(d1) != this.dateTimeFormatter.format(d2);
      }
    }

    /**
     * Keeps track of when the previous hit was sent to determine if a session
     * has expired. Also inspects the `sessionControl` field to handles
     * expiration accordingly.
     * @param {function(!Model)} originalMethod A reference to the overridden
     *     method.
     * @return {function(!Model)}
     */

  }, {
    key: 'sendHitTaskOverride',
    value: function sendHitTaskOverride(originalMethod) {
      var _this = this;

      return function (model) {
        originalMethod(model);

        var sessionControl = model.get('sessionControl');
        var sessionWillStart = sessionControl == 'start' || _this.isExpired();
        var sessionWillEnd = sessionControl == 'end';

        /** @type {SessionStoreData} */
        var sessionData = _this.store.get();
        sessionData.hitTime = Object(__WEBPACK_IMPORTED_MODULE_2__utilities__["g" /* now */])();
        if (sessionWillStart) {
          sessionData.isExpired = false;
          sessionData.id = Object(__WEBPACK_IMPORTED_MODULE_2__utilities__["h" /* uuid */])();
        }
        if (sessionWillEnd) {
          sessionData.isExpired = true;
        }
        _this.store.set(sessionData);
      };
    }

    /**
     * Restores the tracker's original `sendHitTask` to the state before
     * session control was initialized and removes this instance from the global
     * store.
     */

  }, {
    key: 'destroy',
    value: function destroy() {
      __WEBPACK_IMPORTED_MODULE_0__method_chain__["a" /* default */].remove(this.tracker, 'sendHitTask', this.sendHitTaskOverride);
      this.store.destroy();
      delete instances[this.tracker.get('trackingId')];
    }
  }]);

  return Session;
}();

/* harmony default export */ __webpack_exports__["a"] = (Session);


Session.DEFAULT_TIMEOUT = 30; // minutes

/***/ }),

/***/ 194:
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

/**
 * Copyright 2016 Google Inc. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
 * An simple reimplementation of the native Node.js EventEmitter class.
 * The goal of this implementation is to be as small as possible.
 */
var EventEmitter = function () {
  /**
   * Creates the event registry.
   */
  function EventEmitter() {
    _classCallCheck(this, EventEmitter);

    this.registry_ = {};
  }

  /**
   * Adds a handler function to the registry for the passed event.
   * @param {string} event The event name.
   * @param {!Function} fn The handler to be invoked when the passed
   *     event is emitted.
   */


  _createClass(EventEmitter, [{
    key: "on",
    value: function on(event, fn) {
      this.getRegistry_(event).push(fn);
    }

    /**
     * Removes a handler function from the registry for the passed event.
     * @param {string=} event The event name.
     * @param {Function=} fn The handler to be removed.
     */

  }, {
    key: "off",
    value: function off() {
      var event = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : undefined;
      var fn = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : undefined;

      if (event && fn) {
        var eventRegistry = this.getRegistry_(event);
        var handlerIndex = eventRegistry.indexOf(fn);
        if (handlerIndex > -1) {
          eventRegistry.splice(handlerIndex, 1);
        }
      } else {
        this.registry_ = {};
      }
    }

    /**
     * Runs all registered handlers for the passed event with the optional args.
     * @param {string} event The event name.
     * @param {...*} args The arguments to be passed to the handler.
     */

  }, {
    key: "emit",
    value: function emit(event) {
      for (var _len = arguments.length, args = Array(_len > 1 ? _len - 1 : 0), _key = 1; _key < _len; _key++) {
        args[_key - 1] = arguments[_key];
      }

      this.getRegistry_(event).forEach(function (fn) {
        return fn.apply(undefined, args);
      });
    }

    /**
     * Returns the total number of event handlers currently registered.
     * @return {number}
     */

  }, {
    key: "getEventCount",
    value: function getEventCount() {
      var _this = this;

      var eventCount = 0;
      Object.keys(this.registry_).forEach(function (event) {
        eventCount += _this.getRegistry_(event).length;
      });
      return eventCount;
    }

    /**
     * Returns an array of handlers associated with the passed event name.
     * If no handlers have been registered, an empty array is returned.
     * @private
     * @param {string} event The event name.
     * @return {!Array} An array of handler functions.
     */

  }, {
    key: "getRegistry_",
    value: function getRegistry_(event) {
      return this.registry_[event] = this.registry_[event] || [];
    }
  }]);

  return EventEmitter;
}();

/* harmony default export */ __webpack_exports__["a"] = (EventEmitter);

/***/ }),

/***/ 20:
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.parseUrl = exports.parents = exports.matches = exports.getAttributes = exports.dispatch = exports.delegate = exports.closest = undefined;

var _closest = __webpack_require__(146);

var _closest2 = _interopRequireDefault(_closest);

var _delegate = __webpack_require__(187);

var _delegate2 = _interopRequireDefault(_delegate);

var _dispatch = __webpack_require__(188);

var _dispatch2 = _interopRequireDefault(_dispatch);

var _getAttributes = __webpack_require__(189);

var _getAttributes2 = _interopRequireDefault(_getAttributes);

var _matches = __webpack_require__(21);

var _matches2 = _interopRequireDefault(_matches);

var _parents = __webpack_require__(147);

var _parents2 = _interopRequireDefault(_parents);

var _parseUrl = __webpack_require__(190);

var _parseUrl2 = _interopRequireDefault(_parseUrl);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

exports.closest = _closest2.default;
exports.delegate = _delegate2.default;
exports.dispatch = _dispatch2.default;
exports.getAttributes = _getAttributes2.default;
exports.matches = _matches2.default;
exports.parents = _parents2.default;
exports.parseUrl = _parseUrl2.default;

/***/ }),

/***/ 21:
/***/ (function(module, exports, __webpack_require__) {

"use strict";


Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = matches;
var proto = window.Element.prototype;
var nativeMatches = proto.matches || proto.matchesSelector || proto.webkitMatchesSelector || proto.mozMatchesSelector || proto.msMatchesSelector || proto.oMatchesSelector;

/**
 * Tests if a DOM elements matches any of the test DOM elements or selectors.
 * @param {Element} element The DOM element to test.
 * @param {Element|string|Array<Element|string>} test A DOM element, a CSS
 *     selector, or an array of DOM elements or CSS selectors to match against.
 * @return {boolean} True of any part of the test matches.
 */
function matches(element, test) {
  // Validate input.
  if (element && element.nodeType == 1 && test) {
    // if test is a string or DOM element test it.
    if (typeof test == 'string' || test.nodeType == 1) {
      return element == test || matchesSelector(element, /** @type {string} */test);
    } else if ('length' in test) {
      // if it has a length property iterate over the items
      // and return true if any match.
      for (var i = 0, item; item = test[i]; i++) {
        if (element == item || matchesSelector(element, item)) return true;
      }
    }
  }
  // Still here? Return false
  return false;
}

/**
 * Tests whether a DOM element matches a selector. This polyfills the native
 * Element.prototype.matches method across browsers.
 * @param {!Element} element The DOM element to test.
 * @param {string} selector The CSS selector to test element against.
 * @return {boolean} True if the selector matches.
 */
function matchesSelector(element, selector) {
  if (typeof selector != 'string') return false;
  if (nativeMatches) return nativeMatches.call(element, selector);
  var nodes = element.parentNode.querySelectorAll(selector);
  for (var i = 0, node; node = nodes[i]; i++) {
    if (node == element) return true;
  }
  return false;
}

/***/ }),

/***/ 22:
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony export (immutable) */ __webpack_exports__["a"] = provide;
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0__constants__ = __webpack_require__(10);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1__utilities__ = __webpack_require__(3);
/**
 * Copyright 2016 Google Inc. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */




/**
 * Provides a plugin for use with analytics.js, accounting for the possibility
 * that the global command queue has been renamed or not yet defined.
 * @param {string} pluginName The plugin name identifier.
 * @param {Function} pluginConstructor The plugin constructor function.
 */
function provide(pluginName, pluginConstructor) {
  var gaAlias = window.GoogleAnalyticsObject || 'ga';
  window[gaAlias] = window[gaAlias] || function () {
    for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
      args[_key] = arguments[_key];
    }

    (window[gaAlias].q = window[gaAlias].q || []).push(args);
  };

  // Adds the autotrack dev ID if not already included.
  window.gaDevIds = window.gaDevIds || [];
  if (window.gaDevIds.indexOf(__WEBPACK_IMPORTED_MODULE_0__constants__["a" /* DEV_ID */]) < 0) {
    window.gaDevIds.push(__WEBPACK_IMPORTED_MODULE_0__constants__["a" /* DEV_ID */]);
  }

  // Formally provides the plugin for use with analytics.js.
  window[gaAlias]('provide', pluginName, pluginConstructor);

  // Registers the plugin on the global gaplugins object.
  window.gaplugins = window.gaplugins || {};
  window.gaplugins[Object(__WEBPACK_IMPORTED_MODULE_1__utilities__["b" /* capitalize */])(pluginName)] = pluginConstructor;
}

/***/ }),

/***/ 23:
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "a", function() { return plugins; });
/* harmony export (immutable) */ __webpack_exports__["b"] = trackUsage;
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0__constants__ = __webpack_require__(10);
/**
 * Copyright 2016 Google Inc. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */



var plugins = {
  CLEAN_URL_TRACKER: 1,
  EVENT_TRACKER: 2,
  IMPRESSION_TRACKER: 3,
  MEDIA_QUERY_TRACKER: 4,
  OUTBOUND_FORM_TRACKER: 5,
  OUTBOUND_LINK_TRACKER: 6,
  PAGE_VISIBILITY_TRACKER: 7,
  SOCIAL_WIDGET_TRACKER: 8,
  URL_CHANGE_TRACKER: 9,
  MAX_SCROLL_TRACKER: 10
};

var PLUGIN_COUNT = Object.keys(plugins).length;

/**
 * Tracks the usage of the passed plugin by encoding a value into a usage
 * string sent with all hits for the passed tracker.
 * @param {!Tracker} tracker The analytics.js tracker object.
 * @param {number} plugin The plugin enum.
 */
function trackUsage(tracker, plugin) {
  trackVersion(tracker);
  trackPlugin(tracker, plugin);
}

/**
 * Converts a hexadecimal string to a binary string.
 * @param {string} hex A hexadecimal numeric string.
 * @return {string} a binary numeric string.
 */
function convertHexToBin(hex) {
  return parseInt(hex || '0', 16).toString(2);
}

/**
 * Converts a binary string to a hexadecimal string.
 * @param {string} bin A binary numeric string.
 * @return {string} a hexadecimal numeric string.
 */
function convertBinToHex(bin) {
  return parseInt(bin || '0', 2).toString(16);
}

/**
 * Adds leading zeros to a string if it's less than a minimum length.
 * @param {string} str A string to pad.
 * @param {number} len The minimum length of the string
 * @return {string} The padded string.
 */
function padZeros(str, len) {
  if (str.length < len) {
    var toAdd = len - str.length;
    while (toAdd) {
      str = '0' + str;
      toAdd--;
    }
  }
  return str;
}

/**
 * Accepts a binary numeric string and flips the digit from 0 to 1 at the
 * specified index.
 * @param {string} str The binary numeric string.
 * @param {number} index The index to flip the bit.
 * @return {string} The new binary string with the bit flipped on
 */
function flipBitOn(str, index) {
  return str.substr(0, index) + 1 + str.substr(index + 1);
}

/**
 * Accepts a tracker and a plugin index and flips the bit at the specified
 * index on the tracker's usage parameter.
 * @param {Object} tracker An analytics.js tracker.
 * @param {number} pluginIndex The index of the plugin in the global list.
 */
function trackPlugin(tracker, pluginIndex) {
  var usageHex = tracker.get('&' + __WEBPACK_IMPORTED_MODULE_0__constants__["c" /* USAGE_PARAM */]);
  var usageBin = padZeros(convertHexToBin(usageHex), PLUGIN_COUNT);

  // Flip the bit of the plugin being tracked.
  usageBin = flipBitOn(usageBin, PLUGIN_COUNT - pluginIndex);

  // Stores the modified usage string back on the tracker.
  tracker.set('&' + __WEBPACK_IMPORTED_MODULE_0__constants__["c" /* USAGE_PARAM */], convertBinToHex(usageBin));
}

/**
 * Accepts a tracker and adds the current version to the version param.
 * @param {Object} tracker An analytics.js tracker.
 */
function trackVersion(tracker) {
  tracker.set('&' + __WEBPACK_IMPORTED_MODULE_0__constants__["e" /* VERSION_PARAM */], __WEBPACK_IMPORTED_MODULE_0__constants__["d" /* VERSION */]);
}

/***/ }),

/***/ 3:
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
/* harmony export (immutable) */ __webpack_exports__["c"] = createFieldsObj;
/* harmony export (immutable) */ __webpack_exports__["e"] = getAttributeFields;
/* unused harmony export domReady */
/* unused harmony export debounce */
/* harmony export (immutable) */ __webpack_exports__["i"] = withTimeout;
/* harmony export (immutable) */ __webpack_exports__["d"] = deferUntilPluginsLoaded;
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "a", function() { return assign; });
/* unused harmony export camelCase */
/* harmony export (immutable) */ __webpack_exports__["b"] = capitalize;
/* harmony export (immutable) */ __webpack_exports__["f"] = isObject;
/* unused harmony export toArray */
/* harmony export (immutable) */ __webpack_exports__["g"] = now;
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "h", function() { return uuid; });
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_dom_utils__ = __webpack_require__(20);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_0_dom_utils___default = __webpack_require__.n(__WEBPACK_IMPORTED_MODULE_0_dom_utils__);
/* harmony import */ var __WEBPACK_IMPORTED_MODULE_1__method_chain__ = __webpack_require__(11);
var _typeof = typeof Symbol === "function" && typeof Symbol.iterator === "symbol" ? function (obj) { return typeof obj; } : function (obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; };

/**
 * Copyright 2016 Google Inc. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */




/**
 * Accepts default and user override fields and an optional tracker, hit
 * filter, and target element and returns a single object that can be used in
 * `ga('send', ...)` commands.
 * @param {FieldsObj} defaultFields The default fields to return.
 * @param {FieldsObj} userFields Fields set by the user to override the
 *     defaults.
 * @param {Tracker=} tracker The tracker object to apply the hit filter to.
 * @param {Function=} hitFilter A filter function that gets
 *     called with the tracker model right before the `buildHitTask`. It can
 *     be used to modify the model for the current hit only.
 * @param {Element=} target If the hit originated from an interaction
 *     with a DOM element, hitFilter is invoked with that element as the
 *     second argument.
 * @param {(Event|TwttrEvent)=} event If the hit originated via a DOM event,
 *     hitFilter is invoked with that event as the third argument.
 * @return {!FieldsObj} The final fields object.
 */
function createFieldsObj(defaultFields, userFields) {
  var tracker = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : undefined;
  var hitFilter = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : undefined;
  var target = arguments.length > 4 && arguments[4] !== undefined ? arguments[4] : undefined;
  var event = arguments.length > 5 && arguments[5] !== undefined ? arguments[5] : undefined;

  if (typeof hitFilter == 'function') {
    var originalBuildHitTask = tracker.get('buildHitTask');
    return {
      buildHitTask: function buildHitTask( /** @type {!Model} */model) {
        model.set(defaultFields, null, true);
        model.set(userFields, null, true);
        hitFilter(model, target, event);
        originalBuildHitTask(model);
      }
    };
  } else {
    return assign({}, defaultFields, userFields);
  }
}

/**
 * Retrieves the attributes from an DOM element and returns a fields object
 * for all attributes matching the passed prefix string.
 * @param {Element} element The DOM element to get attributes from.
 * @param {string} prefix An attribute prefix. Only the attributes matching
 *     the prefix will be returned on the fields object.
 * @return {FieldsObj} An object of analytics.js fields and values
 */
function getAttributeFields(element, prefix) {
  var attributes = Object(__WEBPACK_IMPORTED_MODULE_0_dom_utils__["getAttributes"])(element);
  var attributeFields = {};

  Object.keys(attributes).forEach(function (attribute) {
    // The `on` prefix is used for event handling but isn't a field.
    if (attribute.indexOf(prefix) === 0 && attribute != prefix + 'on') {
      var value = attributes[attribute];

      // Detects Boolean value strings.
      if (value == 'true') value = true;
      if (value == 'false') value = false;

      var field = camelCase(attribute.slice(prefix.length));
      attributeFields[field] = value;
    }
  });

  return attributeFields;
}

/**
 * Accepts a function to be invoked once the DOM is ready. If the DOM is
 * already ready, the callback is invoked immediately.
 * @param {!Function} callback The ready callback.
 */
function domReady(callback) {
  if (document.readyState == 'loading') {
    document.addEventListener('DOMContentLoaded', function fn() {
      document.removeEventListener('DOMContentLoaded', fn);
      callback();
    });
  } else {
    callback();
  }
}

/**
 * Returns a function, that, as long as it continues to be called, will not
 * actually run. The function will only run after it stops being called for
 * `wait` milliseconds.
 * @param {!Function} fn The function to debounce.
 * @param {number} wait The debounce wait timeout in ms.
 * @return {!Function} The debounced function.
 */
function debounce(fn, wait) {
  var timeout = void 0;
  return function () {
    for (var _len = arguments.length, args = Array(_len), _key = 0; _key < _len; _key++) {
      args[_key] = arguments[_key];
    }

    clearTimeout(timeout);
    timeout = setTimeout(function () {
      return fn.apply(undefined, args);
    }, wait);
  };
}

/**
 * Accepts a function and returns a wrapped version of the function that is
 * expected to be called elsewhere in the system. If it's not called
 * elsewhere after the timeout period, it's called regardless. The wrapper
 * function also prevents the callback from being called more than once.
 * @param {!Function} callback The function to call.
 * @param {number=} wait How many milliseconds to wait before invoking
 *     the callback.
 * @return {!Function} The wrapped version of the passed function.
 */
function withTimeout(callback) {
  var wait = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 2000;

  var called = false;
  var fn = function fn() {
    if (!called) {
      called = true;
      callback();
    }
  };
  setTimeout(fn, wait);
  return fn;
}

// Maps trackers to queue by tracking ID.
var queueMap = {};

/**
 * Queues a function for execution in the next call stack, or immediately
 * before any send commands are executed on the tracker. This allows
 * autotrack plugins to defer running commands until after all other plugins
 * are required but before any other hits are sent.
 * @param {!Tracker} tracker
 * @param {!Function} fn
 */
function deferUntilPluginsLoaded(tracker, fn) {
  var trackingId = tracker.get('trackingId');
  var ref = queueMap[trackingId] = queueMap[trackingId] || {};

  var processQueue = function processQueue() {
    clearTimeout(ref.timeout);
    if (ref.send) {
      __WEBPACK_IMPORTED_MODULE_1__method_chain__["a" /* default */].remove(tracker, 'send', ref.send);
    }
    delete queueMap[trackingId];

    ref.queue.forEach(function (fn) {
      return fn();
    });
  };

  clearTimeout(ref.timeout);
  ref.timeout = setTimeout(processQueue, 0);
  ref.queue = ref.queue || [];
  ref.queue.push(fn);

  if (!ref.send) {
    ref.send = function (originalMethod) {
      return function () {
        processQueue();
        originalMethod.apply(undefined, arguments);
      };
    };
    __WEBPACK_IMPORTED_MODULE_1__method_chain__["a" /* default */].add(tracker, 'send', ref.send);
  }
}

/**
 * A small shim of Object.assign that aims for brevity over spec-compliant
 * handling all the edge cases.
 * @param {!Object} target The target object to assign to.
 * @param {...?Object} sources Additional objects who properties should be
 *     assigned to target. Non-objects are converted to objects.
 * @return {!Object} The modified target object.
 */
var assign = Object.assign || function (target) {
  for (var _len2 = arguments.length, sources = Array(_len2 > 1 ? _len2 - 1 : 0), _key2 = 1; _key2 < _len2; _key2++) {
    sources[_key2 - 1] = arguments[_key2];
  }

  for (var i = 0, len = sources.length; i < len; i++) {
    var source = Object(sources[i]);
    for (var key in source) {
      if (Object.prototype.hasOwnProperty.call(source, key)) {
        target[key] = source[key];
      }
    }
  }
  return target;
};

/**
 * Accepts a string containing hyphen or underscore word separators and
 * converts it to camelCase.
 * @param {string} str The string to camelCase.
 * @return {string} The camelCased version of the string.
 */
function camelCase(str) {
  return str.replace(/[\-\_]+(\w?)/g, function (match, p1) {
    return p1.toUpperCase();
  });
}

/**
 * Capitalizes the first letter of a string.
 * @param {string} str The input string.
 * @return {string} The capitalized string
 */
function capitalize(str) {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

/**
 * Indicates whether the passed variable is a JavaScript object.
 * @param {*} value The input variable to test.
 * @return {boolean} Whether or not the test is an object.
 */
function isObject(value) {
  return (typeof value === 'undefined' ? 'undefined' : _typeof(value)) == 'object' && value !== null;
}

/**
 * Accepts a value that may or may not be an array. If it is not an array,
 * it is returned as the first item in a single-item array.
 * @param {*} value The value to convert to an array if it is not.
 * @return {!Array} The array-ified value.
 */
function toArray(value) {
  return Array.isArray(value) ? value : [value];
}

/**
 * @return {number} The current date timestamp
 */
function now() {
  return +new Date();
}

/*eslint-disable */
// https://gist.github.com/jed/982883
/** @param {?=} a */
var uuid = function b(a) {
  return a ? (a ^ Math.random() * 16 >> a / 4).toString(16) : ([1e7] + -1e3 + -4e3 + -8e3 + -1e11).replace(/[018]/g, b);
};
/*eslint-enable */

/***/ })

/******/ });