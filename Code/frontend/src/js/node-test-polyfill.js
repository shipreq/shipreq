if (typeof(document.execCommand) === "undefined") {
  document.execCommand = () => {}
}

window.alert = (a) => {console.log(`[window.alert] ${JSON.stringify(a)}`)}

require("es6-symbol/implement")

// jsdom doesn't support innerText
if (typeof HTMLElement !== 'undefined' && !Object.prototype.hasOwnProperty.call(HTMLElement.prototype, 'innerText')) {
  Object.defineProperty(HTMLElement.prototype, 'innerText', {
    get: function() { return this.textContent; },
    set: function(v) { this.textContent = v; },
    configurable: true
  });
}

/**
 * Polyfill for SVGSVGElement.createSVGPoint() in JSDOM environments.
 * Supports .x, .y, and .matrixTransform(matrix).
 */
(function polyfillSVGPoint() {
  if (typeof window === 'undefined' || !window.SVGSVGElement) return;

  // 1. Define the Point class
  function MockSVGPoint(x = 0, y = 0) {
    this.x = x;
    this.y = y;
  }

  // 2. Implement the transformation math:
  // x' = a*x + c*y + e
  // y' = b*x + d*y + f
  MockSVGPoint.prototype.matrixTransform = function(m) {
    const newX = (this.x * (m.a ?? 1)) + (this.y * (m.c ?? 0)) + (m.e ?? 0);
    const newY = (this.x * (m.b ?? 0)) + (this.y * (m.d ?? 1)) + (m.f ?? 0);
    return new MockSVGPoint(newX, newY);
  };

  // 3. Attach createSVGPoint to the SVG prototype
  if (!window.SVGSVGElement.prototype.createSVGPoint) {
    window.SVGSVGElement.prototype.createSVGPoint = function() {
      return new MockSVGPoint(0, 0);
    };
  }

  // 4. Also polyfill createSVGMatrix (required for matrixTransform input)
  if (!window.SVGSVGElement.prototype.createSVGMatrix) {
    window.SVGSVGElement.prototype.createSVGMatrix = function() {
      return { a: 1, b: 0, c: 0, d: 1, e: 0, f: 0 };
    };
  }
})();

/**
 * Expanded Polyfill for SVGSVGElement properties in JSDOM.
 * Handles .height, .width, and .viewBox (baseVal/animVal).
 */
(function polyfillSVGAnimatedProperties() {
  if (typeof window === 'undefined' || !window.SVGSVGElement) return;

  // Polyfill for SVGAnimatedLength (used by width/height)
  function createAnimatedLength(element, attrName) {
    return {
      get baseVal() {
        const val = element.getAttribute(attrName);
        // Note: Returns 0 if attribute is missing or non-numeric
        return { value: parseFloat(val) || 0 };
      },
      get animVal() { return this.baseVal; }
    };
  }

  // Polyfill for SVGAnimatedRect (used by viewBox)
  function createAnimatedRect(element, attrName) {
    return {
      get baseVal() {
        const val = element.getAttribute(attrName) || "";
        const parts = val.split(/[\s,]+/).map(parseFloat);
        return {
          x: parts[0] || 0,
          y: parts[1] || 0,
          width: parts[2] || 0,
          height: parts[3] || 0
        };
      },
      get animVal() { return this.baseVal; }
    };
  }

  // Attach to SVGSVGElement prototype
  const props = {
    width: { get() { return createAnimatedLength(this, 'width'); } },
    height: { get() { return createAnimatedLength(this, 'height'); } },
    viewBox: { get() { return createAnimatedRect(this, 'viewBox'); } }
  };

  Object.defineProperties(window.SVGSVGElement.prototype, props);

  // Keep your previous createSVGPoint and createSVGMatrix mocks here as well...
  if (!window.SVGSVGElement.prototype.createSVGPoint) {
    window.SVGSVGElement.prototype.createSVGPoint = function() {
      return { x: 0, y: 0, matrixTransform: function() { return this; } };
    };
  }
})();

/**
 * Polyfill for getCTM and getScreenCTM in JSDOM.
 */
(function polyfillSVGMatrices() {
  if (typeof window === 'undefined' || !window.SVGSVGElement) return;

  const identityMatrix = function() {
    return {
      a: 1, b: 0, c: 0, d: 1, e: 0, f: 0,
      multiply: function() { return this; },
      inverse: function() { return this; },
      translate: function() { return this; },
      scale: function() { return this; },
      rotate: function() { return this; }
    };
  };

  if (!window.SVGSVGElement.prototype.getCTM) {
    window.SVGSVGElement.prototype.getCTM = identityMatrix;
  }

  if (!window.SVGSVGElement.prototype.getScreenCTM) {
    window.SVGSVGElement.prototype.getScreenCTM = identityMatrix;
  }
})();

/**
 * Enhanced SVGMatrix mock for JSDOM
 */
(function polyfillSVGMatrixMethods() {
  if (typeof window === 'undefined' || !window.SVGSVGElement) return;

  function MockSVGMatrix(a = 1, b = 0, c = 0, d = 1, e = 0, f = 0) {
    this.a = a; this.b = b; this.c = c;
    this.d = d; this.e = e; this.f = f;
  }

  // Returns the inverse matrix.
  // For unit tests, returning the identity or a simple inverse is usually enough.
  MockSVGMatrix.prototype.inverse = function() {
    // Simple 2D matrix inversion math
    const det = this.a * this.d - this.b * this.c;
    if (Math.abs(det) < 1e-10) throw new Error("Matrix is not invertible");

    return new MockSVGMatrix(
      this.d / det,
      -this.b / det,
      -this.c / det,
      this.a / det,
      (this.c * this.f - this.d * this.e) / det,
      (this.b * this.e - this.a * this.f) / det
    );
  };

  MockSVGMatrix.prototype.multiply = function(m) {
    return new MockSVGMatrix(
      this.a * m.a + this.c * m.b,
      this.b * m.a + this.d * m.b,
      this.a * m.c + this.c * m.d,
      this.b * m.c + this.d * m.d,
      this.a * m.e + this.c * m.f + this.e,
      this.b * m.e + this.d * m.f + this.f
    );
  };

  // Identity helpers
  const createIdentity = () => new MockSVGMatrix();

  const proto = window.SVGSVGElement.prototype;
  proto.createSVGMatrix = createIdentity;
  proto.getCTM = createIdentity;
  proto.getScreenCTM = createIdentity;
})();

/**
 * Polyfill for getBBox() in JSDOM.
 */
(function polyfillBBox() {
  if (typeof window === 'undefined' || !window.SVGElement) return;

  // We attach to SVGElement so it covers <g>, <rect>, <path>, etc.
  if (!window.SVGElement.prototype.getBBox) {
    window.SVGElement.prototype.getBBox = function() {
      // Attempt to get attributes, otherwise default to 0
      const width = parseFloat(this.getAttribute('width')) || 0;
      const height = parseFloat(this.getAttribute('height')) || 0;
      const x = parseFloat(this.getAttribute('x')) || 0;
      const y = parseFloat(this.getAttribute('y')) || 0;

      return {
        x: x,
        y: y,
        width: width,
        height: height,
        // Compatibility properties for some libraries
        top: y,
        right: x + width,
        bottom: y + height,
        left: x
      };
    };
  }
})();
