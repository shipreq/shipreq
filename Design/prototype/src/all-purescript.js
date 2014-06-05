var PS = PS || {};
PS.Prelude = (function () {
    "use strict";
    var Unit = function (value0) {
        return {
            ctor: "Prelude.Unit", 
            values: [ value0 ]
        };
    };
    var LT = {
        ctor: "Prelude.LT", 
        values: [  ]
    };
    var GT = {
        ctor: "Prelude.GT", 
        values: [  ]
    };
    var EQ = {
        ctor: "Prelude.EQ", 
        values: [  ]
    };
    function cons(e) {  return function (l) {    return [e].concat(l);  };};
    function showStringImpl(s) {  return JSON.stringify(s);};
    function showNumberImpl(n) {  return n.toString();};
    function showArrayImpl (f) {  return function (xs) {    var ss = [];    for (var i = 0, l = xs.length; i < l; i++) {      ss[i] = f(xs[i]);    }    return '[' + ss.join(',') + ']';  };};
    function numAdd(n1) {  return function(n2) {    return n1 + n2;  };};
    function numSub(n1) {  return function(n2) {    return n1 - n2;  };};
    function numMul(n1) {  return function(n2) {    return n1 * n2;  };};
    function numDiv(n1) {  return function(n2) {    return n1 / n2;  };};
    function numMod(n1) {  return function(n2) {    return n1 % n2;  };};
    function numNegate(n) {  return -n;};
    function refEq(r1) {  return function(r2) {    return r1 === r2;  };};
    function refIneq(r1) {  return function(r2) {    return r1 !== r2;  };};
    function unsafeCompare(n1) {  return function(n2) {    return n1 < n2 ? LT : n1 > n2 ? GT : EQ;  };};
    function numShl(n1) {  return function(n2) {    return n1 << n2;  };};
    function numShr(n1) {  return function(n2) {    return n1 >> n2;  };};
    function numZshr(n1) {  return function(n2) {    return n1 >>> n2;  };};
    function numAnd(n1) {  return function(n2) {    return n1 & n2;  };};
    function numOr(n1) {  return function(n2) {    return n1 | n2;  };};
    function numXor(n1) {  return function(n2) {    return n1 ^ n2;  };};
    function numComplement(n) {  return ~n;};
    function boolAnd(b1) {  return function(b2) {    return b1 && b2;  };};
    function boolOr(b1) {  return function(b2) {    return b1 || b2;  };};
    function boolNot(b) {  return !b;};
    function concatString(s1) {  return function(s2) {    return s1 + s2;  };};
    var $bar$bar = function (dict) {
        return dict["||"];
    };
    var $bar = function (dict) {
        return dict["|"];
    };
    var $up = function (dict) {
        return dict["^"];
    };
    var $greater$greater$eq = function (dict) {
        return dict[">>="];
    };
    var $eq$eq = function (dict) {
        return dict["=="];
    };
    var $less$bar$greater = function (dict) {
        return dict["<|>"];
    };
    var $less$greater = function (dict) {
        return dict["<>"];
    };
    var $less$less$less = function (dict) {
        return dict["<<<"];
    };
    var $greater$greater$greater = function (__dict_Semigroupoid_0) {
        return function (f) {
            return function (g) {
                return $less$less$less(__dict_Semigroupoid_0)(g)(f);
            };
        };
    };
    var $less$times$greater = function (dict) {
        return dict["<*>"];
    };
    var $less$dollar$greater = function (dict) {
        return dict["<$>"];
    };
    var $colon = cons;
    var $div$eq = function (dict) {
        return dict["/="];
    };
    var $div = function (dict) {
        return dict["/"];
    };
    var $minus = function (dict) {
        return dict["-"];
    };
    var $plus$plus = function (__dict_Semigroup_1) {
        return $less$greater(__dict_Semigroup_1);
    };
    var $plus = function (dict) {
        return dict["+"];
    };
    var $times = function (dict) {
        return dict["*"];
    };
    var $amp$amp = function (dict) {
        return dict["&&"];
    };
    var $amp = function (dict) {
        return dict["&"];
    };
    var $percent = function (dict) {
        return dict["%"];
    };
    var $dollar = function (f) {
        return function (x) {
            return f(x);
        };
    };
    var $hash = function (x) {
        return function (f) {
            return f(x);
        };
    };
    var zshr = function (dict) {
        return dict.zshr;
    };
    var unit = Unit({});
    var shr = function (dict) {
        return dict.shr;
    };
    var showUnit = function (_) {
        return {
            "__superclasses": {}, 
            show: function (_9) {
                return "Unit {}";
            }
        };
    };
    var showString = function (_) {
        return {
            "__superclasses": {}, 
            show: showStringImpl
        };
    };
    var showOrdering = function (_) {
        return {
            "__superclasses": {}, 
            show: function (_19) {
                if (_19.ctor === "Prelude.LT") {
                    return "LT";
                };
                if (_19.ctor === "Prelude.GT") {
                    return "GT";
                };
                if (_19.ctor === "Prelude.EQ") {
                    return "EQ";
                };
                throw "Failed pattern match";
            }
        };
    };
    var showNumber = function (_) {
        return {
            "__superclasses": {}, 
            show: showNumberImpl
        };
    };
    var showBoolean = function (_) {
        return {
            "__superclasses": {}, 
            show: function (_10) {
                if (_10) {
                    return "true";
                };
                if (!_10) {
                    return "false";
                };
                throw "Failed pattern match";
            }
        };
    };
    var show = function (dict) {
        return dict.show;
    };
    var showArray = function (__dict_Show_2) {
        return {
            "__superclasses": {}, 
            show: showArrayImpl(show(__dict_Show_2))
        };
    };
    var shl = function (dict) {
        return dict.shl;
    };
    var semigroupoidArr = function (_) {
        return {
            "__superclasses": {}, 
            "<<<": function (f) {
                return function (g) {
                    return function (x) {
                        return f(g(x));
                    };
                };
            }
        };
    };
    var semigroupUnit = function (_) {
        return {
            "__superclasses": {}, 
            "<>": function (_26) {
                return function (_27) {
                    return Unit({});
                };
            }
        };
    };
    var semigroupString = function (_) {
        return {
            "__superclasses": {}, 
            "<>": concatString
        };
    };
    var pure = function (dict) {
        return dict.pure;
    };
    var $$return = function (__dict_Monad_3) {
        return pure(__dict_Monad_3["__superclasses"]["Prelude.Applicative_0"]({}));
    };
    var numNumber = function (_) {
        return {
            "__superclasses": {}, 
            "+": numAdd, 
            "-": numSub, 
            "*": numMul, 
            "/": numDiv, 
            "%": numMod, 
            negate: numNegate
        };
    };
    var not = function (dict) {
        return dict.not;
    };
    var negate = function (dict) {
        return dict.negate;
    };
    var liftM1 = function (__dict_Monad_4) {
        return function (f) {
            return function (a) {
                return $greater$greater$eq(__dict_Monad_4["__superclasses"]["Prelude.Bind_1"]({}))(a)(function (_0) {
                    return $$return(__dict_Monad_4)(f(_0));
                });
            };
        };
    };
    var liftA1 = function (__dict_Applicative_5) {
        return function (f) {
            return function (a) {
                return $less$times$greater(__dict_Applicative_5["__superclasses"]["Prelude.Apply_0"]({}))(pure(__dict_Applicative_5)(f))(a);
            };
        };
    };
    var id = function (dict) {
        return dict.id;
    };
    var flip = function (f) {
        return function (b) {
            return function (a) {
                return f(a)(b);
            };
        };
    };
    var eqUnit = function (_) {
        return {
            "__superclasses": {}, 
            "==": function (_11) {
                return function (_12) {
                    return true;
                };
            }, 
            "/=": function (_13) {
                return function (_14) {
                    return false;
                };
            }
        };
    };
    var ordUnit = function (_) {
        return {
            "__superclasses": {
                "Prelude.Eq_0": function (_) {
                    return eqUnit({});
                }
            }, 
            compare: function (_20) {
                return function (_21) {
                    return EQ;
                };
            }
        };
    };
    var eqString = function (_) {
        return {
            "__superclasses": {}, 
            "==": refEq, 
            "/=": refIneq
        };
    };
    var ordString = function (_) {
        return {
            "__superclasses": {
                "Prelude.Eq_0": function (_) {
                    return eqString({});
                }
            }, 
            compare: unsafeCompare
        };
    };
    var eqNumber = function (_) {
        return {
            "__superclasses": {}, 
            "==": refEq, 
            "/=": refIneq
        };
    };
    var ordNumber = function (_) {
        return {
            "__superclasses": {
                "Prelude.Eq_0": function (_) {
                    return eqNumber({});
                }
            }, 
            compare: unsafeCompare
        };
    };
    var eqBoolean = function (_) {
        return {
            "__superclasses": {}, 
            "==": refEq, 
            "/=": refIneq
        };
    };
    var ordBoolean = function (_) {
        return {
            "__superclasses": {
                "Prelude.Eq_0": function (_) {
                    return eqBoolean({});
                }
            }, 
            compare: function (_22) {
                return function (_23) {
                    if (!_22) {
                        if (!_23) {
                            return EQ;
                        };
                    };
                    if (!_22) {
                        if (_23) {
                            return LT;
                        };
                    };
                    if (_22) {
                        if (_23) {
                            return EQ;
                        };
                    };
                    if (_22) {
                        if (!_23) {
                            return GT;
                        };
                    };
                    throw "Failed pattern match";
                };
            }
        };
    };
    var empty = function (dict) {
        return dict.empty;
    };
    var $$const = function (_5) {
        return function (_6) {
            return _5;
        };
    };
    var complement = function (dict) {
        return dict.complement;
    };
    var compare = function (dict) {
        return dict.compare;
    };
    var $less = function (__dict_Ord_8) {
        return function (a1) {
            return function (a2) {
                return (function (_256) {
                    if (_256.ctor === "Prelude.LT") {
                        return true;
                    };
                    return false;
                })(compare(__dict_Ord_8)(a1)(a2));
            };
        };
    };
    var $less$eq = function (__dict_Ord_9) {
        return function (a1) {
            return function (a2) {
                return (function (_257) {
                    if (_257.ctor === "Prelude.GT") {
                        return false;
                    };
                    return true;
                })(compare(__dict_Ord_9)(a1)(a2));
            };
        };
    };
    var $greater = function (__dict_Ord_10) {
        return function (a1) {
            return function (a2) {
                return (function (_258) {
                    if (_258.ctor === "Prelude.GT") {
                        return true;
                    };
                    return false;
                })(compare(__dict_Ord_10)(a1)(a2));
            };
        };
    };
    var $greater$eq = function (__dict_Ord_11) {
        return function (a1) {
            return function (a2) {
                return (function (_259) {
                    if (_259.ctor === "Prelude.LT") {
                        return false;
                    };
                    return true;
                })(compare(__dict_Ord_11)(a1)(a2));
            };
        };
    };
    var categoryArr = function (_) {
        return {
            "__superclasses": {
                "Prelude.Semigroupoid_0": function (_) {
                    return semigroupoidArr({});
                }
            }, 
            id: function (x) {
                return x;
            }
        };
    };
    var boolLikeBoolean = function (_) {
        return {
            "__superclasses": {}, 
            "&&": boolAnd, 
            "||": boolOr, 
            not: boolNot
        };
    };
    var eqArray = function (__dict_Eq_6) {
        return {
            "__superclasses": {}, 
            "==": function (_15) {
                return function (_16) {
                    if (_15.length === 0) {
                        if (_16.length === 0) {
                            return true;
                        };
                    };
                    if (_15.length > 0) {
                        var _265 = _15.slice(1);
                        if (_16.length > 0) {
                            var _263 = _16.slice(1);
                            return $amp$amp(boolLikeBoolean({}))($eq$eq(__dict_Eq_6)(_15[0])(_16[0]))($eq$eq(eqArray(__dict_Eq_6))(_265)(_263));
                        };
                    };
                    return false;
                };
            }, 
            "/=": function (xs) {
                return function (ys) {
                    return not(boolLikeBoolean({}))($eq$eq(eqArray(__dict_Eq_6))(xs)(ys));
                };
            }
        };
    };
    var ordArray = function (__dict_Ord_7) {
        return {
            "__superclasses": {
                "Prelude.Eq_0": function (_) {
                    return eqArray(__dict_Ord_7["__superclasses"]["Prelude.Eq_0"]({}));
                }
            }, 
            compare: function (_24) {
                return function (_25) {
                    if (_24.length === 0) {
                        if (_25.length === 0) {
                            return EQ;
                        };
                    };
                    if (_24.length === 0) {
                        return LT;
                    };
                    if (_25.length === 0) {
                        return GT;
                    };
                    if (_24.length > 0) {
                        var _272 = _24.slice(1);
                        if (_25.length > 0) {
                            var _270 = _25.slice(1);
                            return (function (_268) {
                                if (_268.ctor === "Prelude.EQ") {
                                    return compare(ordArray(__dict_Ord_7))(_272)(_270);
                                };
                                return _268;
                            })(compare(__dict_Ord_7)(_24[0])(_25[0]));
                        };
                    };
                    throw "Failed pattern match";
                };
            }
        };
    };
    var eqOrdering = function (_) {
        return {
            "__superclasses": {}, 
            "==": function (_17) {
                return function (_18) {
                    if (_17.ctor === "Prelude.LT") {
                        if (_18.ctor === "Prelude.LT") {
                            return true;
                        };
                    };
                    if (_17.ctor === "Prelude.GT") {
                        if (_18.ctor === "Prelude.GT") {
                            return true;
                        };
                    };
                    if (_17.ctor === "Prelude.EQ") {
                        if (_18.ctor === "Prelude.EQ") {
                            return true;
                        };
                    };
                    return false;
                };
            }, 
            "/=": function (x) {
                return function (y) {
                    return not(boolLikeBoolean({}))($eq$eq(eqOrdering({}))(x)(y));
                };
            }
        };
    };
    var bitsNumber = function (_) {
        return {
            "__superclasses": {}, 
            "&": numAnd, 
            "|": numOr, 
            "^": numXor, 
            shl: numShl, 
            shr: numShr, 
            zshr: numZshr, 
            complement: numComplement
        };
    };
    var asTypeOf = function (_7) {
        return function (_8) {
            return _7;
        };
    };
    var ap = function (__dict_Monad_12) {
        return function (f) {
            return function (a) {
                return $greater$greater$eq(__dict_Monad_12["__superclasses"]["Prelude.Bind_1"]({}))(f)(function (_2) {
                    return $greater$greater$eq(__dict_Monad_12["__superclasses"]["Prelude.Bind_1"]({}))(a)(function (_1) {
                        return $$return(__dict_Monad_12)(_2(_1));
                    });
                });
            };
        };
    };
    return {
        Unit: Unit, 
        LT: LT, 
        GT: GT, 
        EQ: EQ, 
        unit: unit, 
        "++": $plus$plus, 
        "<>": $less$greater, 
        not: not, 
        "||": $bar$bar, 
        "&&": $amp$amp, 
        complement: complement, 
        zshr: zshr, 
        shr: shr, 
        shl: shl, 
        "^": $up, 
        "|": $bar, 
        "&": $amp, 
        ">=": $greater$eq, 
        "<=": $less$eq, 
        ">": $greater, 
        "<": $less, 
        compare: compare, 
        refIneq: refIneq, 
        refEq: refEq, 
        "/=": $div$eq, 
        "==": $eq$eq, 
        negate: negate, 
        "%": $percent, 
        "/": $div, 
        "*": $times, 
        "-": $minus, 
        "+": $plus, 
        ap: ap, 
        liftM1: liftM1, 
        "return": $$return, 
        ">>=": $greater$greater$eq, 
        "<|>": $less$bar$greater, 
        empty: empty, 
        liftA1: liftA1, 
        pure: pure, 
        "<*>": $less$times$greater, 
        "<$>": $less$dollar$greater, 
        show: show, 
        cons: cons, 
        ":": $colon, 
        "#": $hash, 
        "$": $dollar, 
        id: id, 
        ">>>": $greater$greater$greater, 
        "<<<": $less$less$less, 
        asTypeOf: asTypeOf, 
        "const": $$const, 
        flip: flip, 
        semigroupoidArr: semigroupoidArr, 
        categoryArr: categoryArr, 
        showUnit: showUnit, 
        showString: showString, 
        showBoolean: showBoolean, 
        showNumber: showNumber, 
        showArray: showArray, 
        numNumber: numNumber, 
        eqUnit: eqUnit, 
        eqString: eqString, 
        eqNumber: eqNumber, 
        eqBoolean: eqBoolean, 
        eqArray: eqArray, 
        eqOrdering: eqOrdering, 
        showOrdering: showOrdering, 
        ordUnit: ordUnit, 
        ordBoolean: ordBoolean, 
        ordNumber: ordNumber, 
        ordString: ordString, 
        ordArray: ordArray, 
        bitsNumber: bitsNumber, 
        boolLikeBoolean: boolLikeBoolean, 
        semigroupUnit: semigroupUnit, 
        semigroupString: semigroupString
    };
})();
var PS = PS || {};
PS.Prelude_Unsafe = (function () {
    "use strict";
    function unsafeIndex(xs) {  return function(n) {    return xs[n];  };};
    return {
        unsafeIndex: unsafeIndex
    };
})();
var PS = PS || {};
PS.Data_Maybe = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Nothing = {
        ctor: "Data.Maybe.Nothing", 
        values: [  ]
    };
    var Just = function (value0) {
        return {
            ctor: "Data.Maybe.Just", 
            values: [ value0 ]
        };
    };
    var showMaybe = function (__dict_Show_13) {
        return {
            "__superclasses": {}, 
            show: function (_39) {
                if (_39.ctor === "Data.Maybe.Just") {
                    return "Just (" + Prelude.show(__dict_Show_13)(_39.values[0]) + ")";
                };
                if (_39.ctor === "Data.Maybe.Nothing") {
                    return "Nothing";
                };
                throw "Failed pattern match";
            }
        };
    };
    var maybe = function (_28) {
        return function (_29) {
            return function (_30) {
                if (_30.ctor === "Data.Maybe.Nothing") {
                    return _28;
                };
                if (_30.ctor === "Data.Maybe.Just") {
                    return _29(_30.values[0]);
                };
                throw "Failed pattern match";
            };
        };
    };
    var isNothing = maybe(true)(Prelude["const"](false));
    var isJust = maybe(false)(Prelude["const"](true));
    var functorMaybe = function (_) {
        return {
            "__superclasses": {}, 
            "<$>": function (_31) {
                return function (_32) {
                    if (_32.ctor === "Data.Maybe.Just") {
                        return Just(_31(_32.values[0]));
                    };
                    return Nothing;
                };
            }
        };
    };
    var fromMaybe = function (a) {
        return maybe(a)(Prelude.id(Prelude.categoryArr({})));
    };
    var eqMaybe = function (__dict_Eq_15) {
        return {
            "__superclasses": {}, 
            "==": function (_40) {
                return function (_41) {
                    if (_40.ctor === "Data.Maybe.Nothing") {
                        if (_41.ctor === "Data.Maybe.Nothing") {
                            return true;
                        };
                    };
                    if (_40.ctor === "Data.Maybe.Just") {
                        if (_41.ctor === "Data.Maybe.Just") {
                            return Prelude["=="](__dict_Eq_15)(_40.values[0])(_41.values[0]);
                        };
                    };
                    return false;
                };
            }, 
            "/=": function (a) {
                return function (b) {
                    return !Prelude["=="](eqMaybe(__dict_Eq_15))(a)(b);
                };
            }
        };
    };
    var ordMaybe = function (__dict_Ord_14) {
        return {
            "__superclasses": {
                "Prelude.Eq_0": function (_) {
                    return eqMaybe(__dict_Ord_14["__superclasses"]["Prelude.Eq_0"]({}));
                }
            }, 
            compare: function (_42) {
                return function (_43) {
                    if (_42.ctor === "Data.Maybe.Just") {
                        if (_43.ctor === "Data.Maybe.Just") {
                            return Prelude.compare(__dict_Ord_14)(_42.values[0])(_43.values[0]);
                        };
                    };
                    if (_42.ctor === "Data.Maybe.Nothing") {
                        if (_43.ctor === "Data.Maybe.Nothing") {
                            return Prelude.EQ;
                        };
                    };
                    if (_42.ctor === "Data.Maybe.Nothing") {
                        return Prelude.LT;
                    };
                    if (_43.ctor === "Data.Maybe.Nothing") {
                        return Prelude.GT;
                    };
                    throw "Failed pattern match";
                };
            }
        };
    };
    var applyMaybe = function (_) {
        return {
            "__superclasses": {
                "Prelude.Functor_0": function (_) {
                    return functorMaybe({});
                }
            }, 
            "<*>": function (_33) {
                return function (_34) {
                    if (_33.ctor === "Data.Maybe.Just") {
                        return Prelude["<$>"](functorMaybe({}))(_33.values[0])(_34);
                    };
                    if (_33.ctor === "Data.Maybe.Nothing") {
                        return Nothing;
                    };
                    throw "Failed pattern match";
                };
            }
        };
    };
    var bindMaybe = function (_) {
        return {
            "__superclasses": {
                "Prelude.Apply_0": function (_) {
                    return applyMaybe({});
                }
            }, 
            ">>=": function (_37) {
                return function (_38) {
                    if (_37.ctor === "Data.Maybe.Just") {
                        return _38(_37.values[0]);
                    };
                    if (_37.ctor === "Data.Maybe.Nothing") {
                        return Nothing;
                    };
                    throw "Failed pattern match";
                };
            }
        };
    };
    var applicativeMaybe = function (_) {
        return {
            "__superclasses": {
                "Prelude.Apply_0": function (_) {
                    return applyMaybe({});
                }
            }, 
            pure: Just
        };
    };
    var monadMaybe = function (_) {
        return {
            "__superclasses": {
                "Prelude.Applicative_0": function (_) {
                    return applicativeMaybe({});
                }, 
                "Prelude.Bind_1": function (_) {
                    return bindMaybe({});
                }
            }
        };
    };
    var alternativeMaybe = function (_) {
        return {
            "__superclasses": {}, 
            empty: Nothing, 
            "<|>": function (_35) {
                return function (_36) {
                    if (_35.ctor === "Data.Maybe.Nothing") {
                        return _36;
                    };
                    return _35;
                };
            }
        };
    };
    return {
        Nothing: Nothing, 
        Just: Just, 
        isNothing: isNothing, 
        isJust: isJust, 
        fromMaybe: fromMaybe, 
        maybe: maybe, 
        functorMaybe: functorMaybe, 
        applyMaybe: applyMaybe, 
        applicativeMaybe: applicativeMaybe, 
        alternativeMaybe: alternativeMaybe, 
        bindMaybe: bindMaybe, 
        monadMaybe: monadMaybe, 
        showMaybe: showMaybe, 
        eqMaybe: eqMaybe, 
        ordMaybe: ordMaybe
    };
})();
var PS = PS || {};
PS.Data_Maybe_Unsafe = (function () {
    "use strict";
    var fromJust = function (_44) {
        if (_44.ctor === "Data.Maybe.Just") {
            return _44.values[0];
        };
        throw "Failed pattern match";
    };
    return {
        fromJust: fromJust
    };
})();
var PS = PS || {};
PS.Data_Function = (function () {
    "use strict";
    function mkFn0(f) {  return function() {    return f({});  };};
    function mkFn1(f) {  return function(a) {    return f(a);  };};
    function mkFn2(f) {  return function(a, b) {    return f(a)(b);  };};
    function mkFn3(f) {  return function(a, b, c) {    return f(a)(b)(c);  };};
    function mkFn4(f) {  return function(a, b, c, d) {    return f(a)(b)(c)(d);  };};
    function mkFn5(f) {  return function(a, b, c, d, e) {    return f(a)(b)(c)(d)(e);  };};
    function runFn0(f) {  return f();};
    function runFn1(f) {  return function(a) {    return f(a);  };};
    function runFn2(f) {  return function(a) {    return function(b) {      return f(a, b);    };  };};
    function runFn3(f) {  return function(a) {    return function(b) {      return function(c) {        return f(a, b, c);      };    };  };};
    function runFn4(f) {  return function(a) {    return function(b) {      return function(c) {        return function(d) {          return f(a, b, c, d);        };      };    };  };};
    function runFn5(f) {  return function(a) {    return function(b) {      return function(c) {        return function(d) {          return function(e) {            return f(a, b, c, d, e);          };        };      };    };  };};
    var on = function (f) {
        return function (g) {
            return function (x) {
                return function (y) {
                    return f(g(x))(g(y));
                };
            };
        };
    };
    return {
        runFn5: runFn5, 
        runFn4: runFn4, 
        runFn3: runFn3, 
        runFn2: runFn2, 
        runFn1: runFn1, 
        runFn0: runFn0, 
        mkFn5: mkFn5, 
        mkFn4: mkFn4, 
        mkFn3: mkFn3, 
        mkFn2: mkFn2, 
        mkFn1: mkFn1, 
        mkFn0: mkFn0, 
        on: on
    };
})();
var PS = PS || {};
PS.Data_Eq = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Ref = function (value0) {
        return {
            ctor: "Data.Eq.Ref", 
            values: [ value0 ]
        };
    };
    var liftRef = function (_45) {
        return function (_46) {
            return function (_47) {
                return _45(_46.values[0])(_47.values[0]);
            };
        };
    };
    var eqRef = function (_) {
        return {
            "__superclasses": {}, 
            "==": liftRef(Prelude.refEq), 
            "/=": liftRef(Prelude.refIneq)
        };
    };
    return {
        Ref: Ref, 
        liftRef: liftRef, 
        eqRef: eqRef
    };
})();
var PS = PS || {};
PS.Data_Either = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Left = function (value0) {
        return {
            ctor: "Data.Either.Left", 
            values: [ value0 ]
        };
    };
    var Right = function (value0) {
        return {
            ctor: "Data.Either.Right", 
            values: [ value0 ]
        };
    };
    var showEither = function (__dict_Show_16) {
        return function (__dict_Show_17) {
            return {
                "__superclasses": {}, 
                show: function (_55) {
                    if (_55.ctor === "Data.Either.Left") {
                        return "Left (" + Prelude.show(__dict_Show_16)(_55.values[0]) + ")";
                    };
                    if (_55.ctor === "Data.Either.Right") {
                        return "Right (" + Prelude.show(__dict_Show_17)(_55.values[0]) + ")";
                    };
                    throw "Failed pattern match";
                }
            };
        };
    };
    var functorEither = function (_) {
        return {
            "__superclasses": {}, 
            "<$>": function (_51) {
                return function (_52) {
                    if (_52.ctor === "Data.Either.Left") {
                        return Left(_52.values[0]);
                    };
                    if (_52.ctor === "Data.Either.Right") {
                        return Right(_51(_52.values[0]));
                    };
                    throw "Failed pattern match";
                };
            }
        };
    };
    var eqEither = function (__dict_Eq_20) {
        return function (__dict_Eq_21) {
            return {
                "__superclasses": {}, 
                "==": function (_56) {
                    return function (_57) {
                        if (_56.ctor === "Data.Either.Left") {
                            if (_57.ctor === "Data.Either.Left") {
                                return Prelude["=="](__dict_Eq_20)(_56.values[0])(_57.values[0]);
                            };
                        };
                        if (_56.ctor === "Data.Either.Right") {
                            if (_57.ctor === "Data.Either.Right") {
                                return Prelude["=="](__dict_Eq_21)(_56.values[0])(_57.values[0]);
                            };
                        };
                        return false;
                    };
                }, 
                "/=": function (a) {
                    return function (b) {
                        return !Prelude["=="](eqEither(__dict_Eq_20)(__dict_Eq_21))(a)(b);
                    };
                }
            };
        };
    };
    var ordEither = function (__dict_Ord_18) {
        return function (__dict_Ord_19) {
            return {
                "__superclasses": {
                    "Prelude.Eq_0": function (_) {
                        return eqEither(__dict_Ord_18["__superclasses"]["Prelude.Eq_0"]({}))(__dict_Ord_19["__superclasses"]["Prelude.Eq_0"]({}));
                    }
                }, 
                compare: function (_58) {
                    return function (_59) {
                        if (_58.ctor === "Data.Either.Left") {
                            if (_59.ctor === "Data.Either.Left") {
                                return Prelude.compare(__dict_Ord_18)(_58.values[0])(_59.values[0]);
                            };
                        };
                        if (_58.ctor === "Data.Either.Right") {
                            if (_59.ctor === "Data.Either.Right") {
                                return Prelude.compare(__dict_Ord_19)(_58.values[0])(_59.values[0]);
                            };
                        };
                        if (_58.ctor === "Data.Either.Left") {
                            return Prelude.LT;
                        };
                        if (_59.ctor === "Data.Either.Left") {
                            return Prelude.GT;
                        };
                        throw "Failed pattern match";
                    };
                }
            };
        };
    };
    var either = function (_48) {
        return function (_49) {
            return function (_50) {
                if (_50.ctor === "Data.Either.Left") {
                    return _48(_50.values[0]);
                };
                if (_50.ctor === "Data.Either.Right") {
                    return _49(_50.values[0]);
                };
                throw "Failed pattern match";
            };
        };
    };
    var isLeft = either(Prelude["const"](true))(Prelude["const"](false));
    var isRight = either(Prelude["const"](false))(Prelude["const"](true));
    var applyEither = function (_) {
        return {
            "__superclasses": {
                "Prelude.Functor_0": function (_) {
                    return functorEither({});
                }
            }, 
            "<*>": function (_53) {
                return function (_54) {
                    if (_53.ctor === "Data.Either.Left") {
                        return Left(_53.values[0]);
                    };
                    if (_53.ctor === "Data.Either.Right") {
                        return Prelude["<$>"](functorEither({}))(_53.values[0])(_54);
                    };
                    throw "Failed pattern match";
                };
            }
        };
    };
    var bindEither = function (_) {
        return {
            "__superclasses": {
                "Prelude.Apply_0": function (_) {
                    return applyEither({});
                }
            }, 
            ">>=": either(function (e) {
                return function (_) {
                    return Left(e);
                };
            })(function (a) {
                return function (f) {
                    return f(a);
                };
            })
        };
    };
    var applicativeEither = function (_) {
        return {
            "__superclasses": {
                "Prelude.Apply_0": function (_) {
                    return applyEither({});
                }
            }, 
            pure: Right
        };
    };
    var monadEither = function (_) {
        return {
            "__superclasses": {
                "Prelude.Applicative_0": function (_) {
                    return applicativeEither({});
                }, 
                "Prelude.Bind_1": function (_) {
                    return bindEither({});
                }
            }
        };
    };
    return {
        Left: Left, 
        Right: Right, 
        isRight: isRight, 
        isLeft: isLeft, 
        either: either, 
        functorEither: functorEither, 
        applyEither: applyEither, 
        applicativeEither: applicativeEither, 
        bindEither: bindEither, 
        monadEither: monadEither, 
        showEither: showEither, 
        eqEither: eqEither, 
        ordEither: ordEither
    };
})();
var PS = PS || {};
PS.Data_Array = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Maybe = PS.Data_Maybe;
    var Prelude_Unsafe = PS.Prelude_Unsafe;
    function snoc(l) {  return function (e) {    var l1 = l.slice();    l1.push(e);     return l1;  };};
    function length (xs) {  return xs.length;};
    function findIndex (f) {  return function (arr) {    for (var i = 0, l = arr.length; i < l; i++) {      if (f(arr[i])) {        return i;      }    }    return -1;  };};
    function findLastIndex (f) {  return function (arr) {    for (var i = arr.length - 1; i >= 0; i--) {      if (f(arr[i])) {        return i;      }    }    return -1;  };};
    function append (l1) {  return function (l2) {    return l1.concat(l2);  };};
    function concat (xss) {  var result = [];  for (var i = 0, l = xss.length; i < l; i++) {    result.push.apply(result, xss[i]);  }  return result;};
    function reverse (l) {  return l.slice().reverse();};
    function drop (n) {  return function (l) {    return l.slice(n);  };};
    function slice (s) {  return function (e) {    return function (l) {      return l.slice(s, e);    };  };};
    function insertAt (index) {  return function (a) {    return function (l) {      var l1 = l.slice();      l1.splice(index, 0, a);      return l1;    };   };};
    function deleteAt (index) {  return function (n) {    return function (l) {      var l1 = l.slice();      l1.splice(index, n);      return l1;    };   };};
    function updateAt (index) {  return function (a) {    return function (l) {      var i = ~~index;      if (i < 0 || i >= l.length) return l;      var l1 = l.slice();      l1[i] = a;      return l1;    };   };};
    function concatMap (f) {  return function (arr) {    var result = [];    for (var i = 0, l = arr.length; i < l; i++) {      Array.prototype.push.apply(result, f(arr[i]));    }    return result;  };};
    function map (f) {  return function (arr) {    var l = arr.length;    var result = new Array(l);    for (var i = 0; i < l; i++) {      result[i] = f(arr[i]);    }    return result;  };};
    function filter (f) {  return function (arr) {    var n = 0;    var result = [];    for (var i = 0, l = arr.length; i < l; i++) {      if (f(arr[i])) {        result[n++] = arr[i];      }    }    return result;  };};
    function range (start) {  return function (end) {    var i = ~~start, e = ~~end;    var step = i > e ? -1 : 1;    var result = [i], n = 1;    while (i !== e) {      i += step;      result[n++] = i;    }    return result;  };};
    function zipWith (f) {  return function (xs) {    return function (ys) {      var l = xs.length < ys.length ? xs.length : ys.length;      var result = new Array(l);      for (var i = 0; i < l; i++) {        result[i] = f(xs[i])(ys[i]);      }      return result;    };  };};
    function sortJS (f) {  return function (l) {    return l.slice().sort(function (x, y) {      return f(x)(y);    });  };};
    var $bang$bang = function (xs) {
        return function (n) {
            var isInt = function (n) {
                return n !== ~~n;
            };
            return (n < 0 || n >= length(xs) || isInt(n)) ? Data_Maybe.Nothing : Data_Maybe.Just(xs[n]);
        };
    };
    var take = function (n) {
        return slice(0)(n);
    };
    var tail = function (_62) {
        if (_62.length > 0) {
            var _343 = _62.slice(1);
            return Data_Maybe.Just(_343);
        };
        return Data_Maybe.Nothing;
    };
    var span = (function () {
        var go = function (__copy__78) {
            return function (__copy__79) {
                return function (__copy__80) {
                    var _78 = __copy__78;
                    var _79 = __copy__79;
                    var _80 = __copy__80;
                    tco: while (true) {
                        var acc = _78;
                        if (_80.length > 0) {
                            var _348 = _80.slice(1);
                            if (_79(_80[0])) {
                                var __tco__78 = Prelude[":"](_80[0])(acc);
                                var __tco__79 = _79;
                                _78 = __tco__78;
                                _79 = __tco__79;
                                _80 = _348;
                                continue tco;
                            };
                        };
                        return {
                            init: reverse(_78), 
                            rest: _80
                        };
                    };
                };
            };
        };
        return go([  ]);
    })();
    var sortBy = function (comp) {
        return function (xs) {
            var comp$prime = function (x) {
                return function (y) {
                    return (function (_349) {
                        if (_349.ctor === "Prelude.GT") {
                            return 1;
                        };
                        if (_349.ctor === "Prelude.EQ") {
                            return 0;
                        };
                        if (_349.ctor === "Prelude.LT") {
                            return -1;
                        };
                        throw "Failed pattern match";
                    })(comp(x)(y));
                };
            };
            return sortJS(comp$prime)(xs);
        };
    };
    var sort = function (__dict_Ord_22) {
        return function (xs) {
            return sortBy(Prelude.compare(__dict_Ord_22))(xs);
        };
    };
    var singleton = function (a) {
        return [ a ];
    };
    var semigroupArray = function (_) {
        return {
            "__superclasses": {}, 
            "<>": append
        };
    };
    var $$null = function (_64) {
        if (_64.length === 0) {
            return true;
        };
        return false;
    };
    var nubBy = function (_71) {
        return function (_72) {
            if (_72.length === 0) {
                return [  ];
            };
            if (_72.length > 0) {
                var _354 = _72.slice(1);
                return Prelude[":"](_72[0])(nubBy(_71)(filter(function (y) {
                    return !_71(_72[0])(y);
                })(_354)));
            };
            throw "Failed pattern match";
        };
    };
    var nub = function (__dict_Eq_23) {
        return nubBy(Prelude["=="](__dict_Eq_23));
    };
    var mapMaybe = function (f) {
        return concatMap(Prelude["<<<"](Prelude.semigroupoidArr({}))(Data_Maybe.maybe([  ])(singleton))(f));
    };
    var last = function (__copy__61) {
        var _61 = __copy__61;
        tco: while (true) {
            if (_61.length > 0) {
                var _357 = _61.slice(1);
                if (_357.length === 0) {
                    return Data_Maybe.Just(_61[0]);
                };
            };
            if (_61.length > 0) {
                var _359 = _61.slice(1);
                _61 = _359;
                continue tco;
            };
            return Data_Maybe.Nothing;
        };
    };
    var intersectBy = function (_68) {
        return function (_69) {
            return function (_70) {
                if (_69.length === 0) {
                    return [  ];
                };
                if (_70.length === 0) {
                    return [  ];
                };
                var el = function (x) {
                    return findIndex(_68(x))(_70) >= 0;
                };
                return filter(el)(_69);
            };
        };
    };
    var intersect = function (__dict_Eq_24) {
        return intersectBy(Prelude["=="](__dict_Eq_24));
    };
    var init = function (_63) {
        if (_63.length === 0) {
            return Data_Maybe.Nothing;
        };
        return Data_Maybe.Just(slice(0)(length(_63) - 1)(_63));
    };
    var head = function (_60) {
        if (_60.length > 0) {
            return Data_Maybe.Just(_60[0]);
        };
        return Data_Maybe.Nothing;
    };
    var groupBy = (function () {
        var go = function (__copy__75) {
            return function (__copy__76) {
                return function (__copy__77) {
                    var _75 = __copy__75;
                    var _76 = __copy__76;
                    var _77 = __copy__77;
                    tco: while (true) {
                        var acc = _75;
                        if (_77.length === 0) {
                            return reverse(acc);
                        };
                        if (_77.length > 0) {
                            var _371 = _77.slice(1);
                            var sp = span(_76(_77[0]))(_371);
                            var __tco__75 = Prelude[":"](Prelude[":"](_77[0])(sp.init))(_75);
                            var __tco__76 = _76;
                            _75 = __tco__75;
                            _76 = __tco__76;
                            _77 = sp.rest;
                            continue tco;
                        };
                        throw "Failed pattern match";
                    };
                };
            };
        };
        return go([  ]);
    })();
    var group = function (__dict_Eq_25) {
        return function (xs) {
            return groupBy(Prelude["=="](__dict_Eq_25))(xs);
        };
    };
    var group$prime = function (__dict_Ord_26) {
        return Prelude["<<<"](Prelude.semigroupoidArr({}))(group(__dict_Ord_26["__superclasses"]["Prelude.Eq_0"]({})))(sort(__dict_Ord_26));
    };
    var functorArray = function (_) {
        return {
            "__superclasses": {}, 
            "<$>": map
        };
    };
    var elemLastIndex = function (__dict_Eq_27) {
        return function (x) {
            return findLastIndex(Prelude["=="](__dict_Eq_27)(x));
        };
    };
    var elemIndex = function (__dict_Eq_28) {
        return function (x) {
            return findIndex(Prelude["=="](__dict_Eq_28)(x));
        };
    };
    var deleteBy = function (_65) {
        return function (_66) {
            return function (_67) {
                if (_67.length === 0) {
                    return [  ];
                };
                return (function (_375) {
                    if (_375 < 0) {
                        return _67;
                    };
                    return deleteAt(_375)(1)(_67);
                })(findIndex(_65(_66))(_67));
            };
        };
    };
    var $$delete = function (__dict_Eq_29) {
        return deleteBy(Prelude["=="](__dict_Eq_29));
    };
    var $bslash$bslash = function (__dict_Eq_30) {
        return function (xs) {
            return function (ys) {
                var go = function (__copy__73) {
                    return function (__copy__74) {
                        var _73 = __copy__73;
                        var _74 = __copy__74;
                        tco: while (true) {
                            var xs = _73;
                            if (_74.length === 0) {
                                return xs;
                            };
                            if (_73.length === 0) {
                                return [  ];
                            };
                            if (_74.length > 0) {
                                var _379 = _74.slice(1);
                                var __tco__73 = $$delete(__dict_Eq_30)(_74[0])(_73);
                                _73 = __tco__73;
                                _74 = _379;
                                continue tco;
                            };
                            throw "Failed pattern match";
                        };
                    };
                };
                return go(xs)(ys);
            };
        };
    };
    var catMaybes = concatMap(Data_Maybe.maybe([  ])(singleton));
    var applicativeArray = function (_) {
        return {
            "__superclasses": {
                "Prelude.Apply_0": function (_) {
                    return applyArray({});
                }
            }, 
            pure: singleton
        };
    };
    var applyArray = function (_) {
        return {
            "__superclasses": {
                "Prelude.Functor_0": function (_) {
                    return functorArray({});
                }
            }, 
            "<*>": Prelude.ap(monadArray({}))
        };
    };
    var monadArray = function (_) {
        return {
            "__superclasses": {
                "Prelude.Applicative_0": function (_) {
                    return applicativeArray({});
                }, 
                "Prelude.Bind_1": function (_) {
                    return bindArray({});
                }
            }
        };
    };
    var bindArray = function (_) {
        return {
            "__superclasses": {
                "Prelude.Apply_0": function (_) {
                    return applyArray({});
                }
            }, 
            ">>=": Prelude.flip(concatMap)
        };
    };
    var alternativeArray = function (_) {
        return {
            "__superclasses": {}, 
            empty: [  ], 
            "<|>": append
        };
    };
    return {
        span: span, 
        groupBy: groupBy, 
        "group'": group$prime, 
        group: group, 
        sortBy: sortBy, 
        sort: sort, 
        nubBy: nubBy, 
        nub: nub, 
        zipWith: zipWith, 
        range: range, 
        filter: filter, 
        concatMap: concatMap, 
        intersect: intersect, 
        intersectBy: intersectBy, 
        "\\\\": $bslash$bslash, 
        "delete": $$delete, 
        deleteBy: deleteBy, 
        updateAt: updateAt, 
        deleteAt: deleteAt, 
        insertAt: insertAt, 
        take: take, 
        drop: drop, 
        reverse: reverse, 
        concat: concat, 
        append: append, 
        elemLastIndex: elemLastIndex, 
        elemIndex: elemIndex, 
        findLastIndex: findLastIndex, 
        findIndex: findIndex, 
        length: length, 
        catMaybes: catMaybes, 
        mapMaybe: mapMaybe, 
        map: map, 
        "null": $$null, 
        init: init, 
        tail: tail, 
        last: last, 
        head: head, 
        singleton: singleton, 
        snoc: snoc, 
        "!!": $bang$bang, 
        functorArray: functorArray, 
        applyArray: applyArray, 
        applicativeArray: applicativeArray, 
        bindArray: bindArray, 
        monadArray: monadArray, 
        semigroupArray: semigroupArray, 
        alternativeArray: alternativeArray
    };
})();
var PS = PS || {};
PS.Data_Array_Unsafe = (function () {
    "use strict";
    var Prelude_Unsafe = PS.Prelude_Unsafe;
    var Prelude = PS.Prelude;
    var Data_Array = PS.Data_Array;
    var Data_Maybe_Unsafe = PS.Data_Maybe_Unsafe;
    var tail = function (_82) {
        if (_82.length > 0) {
            var _382 = _82.slice(1);
            return _382;
        };
        throw "Failed pattern match";
    };
    var last = function (xs) {
        return xs[Data_Array.length(xs) - 1];
    };
    var init = Prelude["<<<"](Prelude.semigroupoidArr({}))(Data_Maybe_Unsafe.fromJust)(Data_Array.init);
    var head = function (_81) {
        if (_81.length > 0) {
            return _81[0];
        };
        throw "Failed pattern match";
    };
    return {
        init: init, 
        last: last, 
        tail: tail, 
        head: head
    };
})();
var PS = PS || {};
PS.Data_Monoid = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Array = PS.Data_Array;
    var monoidUnit = function (_) {
        return {
            "__superclasses": {
                "Prelude.Semigroup_0": function (_) {
                    return Prelude.semigroupUnit({});
                }
            }, 
            mempty: Prelude.unit
        };
    };
    var monoidString = function (_) {
        return {
            "__superclasses": {
                "Prelude.Semigroup_0": function (_) {
                    return Prelude.semigroupString({});
                }
            }, 
            mempty: ""
        };
    };
    var monoidArray = function (_) {
        return {
            "__superclasses": {
                "Prelude.Semigroup_0": function (_) {
                    return Data_Array.semigroupArray({});
                }
            }, 
            mempty: [  ]
        };
    };
    var mempty = function (dict) {
        return dict.mempty;
    };
    return {
        mempty: mempty, 
        monoidString: monoidString, 
        monoidArray: monoidArray, 
        monoidUnit: monoidUnit
    };
})();
var PS = PS || {};
PS.Data_Monoid_All = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var All = function (value0) {
        return {
            ctor: "Data.Monoid.All.All", 
            values: [ value0 ]
        };
    };
    var showAll = function (_) {
        return {
            "__superclasses": {}, 
            show: function (_88) {
                return "All (" + Prelude.show(Prelude.showBoolean({}))(_88.values[0]) + ")";
            }
        };
    };
    var semigroupAll = function (_) {
        return {
            "__superclasses": {}, 
            "<>": function (_89) {
                return function (_90) {
                    return All(_89.values[0] && _90.values[0]);
                };
            }
        };
    };
    var runAll = function (_83) {
        return _83.values[0];
    };
    var monoidAll = function (_) {
        return {
            "__superclasses": {
                "Prelude.Semigroup_0": function (_) {
                    return semigroupAll({});
                }
            }, 
            mempty: All(true)
        };
    };
    var eqAll = function (_) {
        return {
            "__superclasses": {}, 
            "==": function (_84) {
                return function (_85) {
                    return _84.values[0] === _85.values[0];
                };
            }, 
            "/=": function (_86) {
                return function (_87) {
                    return _86.values[0] !== _87.values[0];
                };
            }
        };
    };
    return {
        All: All, 
        runAll: runAll, 
        eqAll: eqAll, 
        showAll: showAll, 
        semigroupAll: semigroupAll, 
        monoidAll: monoidAll
    };
})();
var PS = PS || {};
PS.Data_Monoid_Any = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Any = function (value0) {
        return {
            ctor: "Data.Monoid.Any.Any", 
            values: [ value0 ]
        };
    };
    var showAny = function (_) {
        return {
            "__superclasses": {}, 
            show: function (_96) {
                return "Any (" + Prelude.show(Prelude.showBoolean({}))(_96.values[0]) + ")";
            }
        };
    };
    var semigroupAny = function (_) {
        return {
            "__superclasses": {}, 
            "<>": function (_97) {
                return function (_98) {
                    return Any(_97.values[0] || _98.values[0]);
                };
            }
        };
    };
    var runAny = function (_91) {
        return _91.values[0];
    };
    var monoidAny = function (_) {
        return {
            "__superclasses": {
                "Prelude.Semigroup_0": function (_) {
                    return semigroupAny({});
                }
            }, 
            mempty: Any(false)
        };
    };
    var eqAny = function (_) {
        return {
            "__superclasses": {}, 
            "==": function (_92) {
                return function (_93) {
                    return _92.values[0] === _93.values[0];
                };
            }, 
            "/=": function (_94) {
                return function (_95) {
                    return _94.values[0] !== _95.values[0];
                };
            }
        };
    };
    return {
        Any: Any, 
        runAny: runAny, 
        eqAny: eqAny, 
        showAny: showAny, 
        semigroupAny: semigroupAny, 
        monoidAny: monoidAny
    };
})();
var PS = PS || {};
PS.Data_Monoid_Dual = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Monoid = PS.Data_Monoid;
    var Dual = function (value0) {
        return {
            ctor: "Data.Monoid.Dual.Dual", 
            values: [ value0 ]
        };
    };
    var showDual = function (__dict_Show_31) {
        return {
            "__superclasses": {}, 
            show: function (_106) {
                return "Dual (" + Prelude.show(__dict_Show_31)(_106.values[0]) + ")";
            }
        };
    };
    var semigroupDual = function (__dict_Semigroup_32) {
        return {
            "__superclasses": {}, 
            "<>": function (_107) {
                return function (_108) {
                    return Dual(Prelude["<>"](__dict_Semigroup_32)(_108.values[0])(_107.values[0]));
                };
            }
        };
    };
    var runDual = function (_99) {
        return _99.values[0];
    };
    var monoidDual = function (__dict_Monoid_34) {
        return {
            "__superclasses": {
                "Prelude.Semigroup_0": function (_) {
                    return semigroupDual(__dict_Monoid_34["__superclasses"]["Prelude.Semigroup_0"]({}));
                }
            }, 
            mempty: Dual(Data_Monoid.mempty(__dict_Monoid_34))
        };
    };
    var eqDual = function (__dict_Eq_35) {
        return {
            "__superclasses": {}, 
            "==": function (_100) {
                return function (_101) {
                    return Prelude["=="](__dict_Eq_35)(_100.values[0])(_101.values[0]);
                };
            }, 
            "/=": function (_102) {
                return function (_103) {
                    return Prelude["/="](__dict_Eq_35)(_102.values[0])(_103.values[0]);
                };
            }
        };
    };
    var ordDual = function (__dict_Ord_33) {
        return {
            "__superclasses": {
                "Prelude.Eq_0": function (_) {
                    return eqDual(__dict_Ord_33["__superclasses"]["Prelude.Eq_0"]({}));
                }
            }, 
            compare: function (_104) {
                return function (_105) {
                    return Prelude.compare(__dict_Ord_33)(_104.values[0])(_105.values[0]);
                };
            }
        };
    };
    return {
        Dual: Dual, 
        runDual: runDual, 
        eqDual: eqDual, 
        ordDual: ordDual, 
        showDual: showDual, 
        semigroupDual: semigroupDual, 
        monoidDual: monoidDual
    };
})();
var PS = PS || {};
PS.Data_Monoid_Endo = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Endo = function (value0) {
        return {
            ctor: "Data.Monoid.Endo.Endo", 
            values: [ value0 ]
        };
    };
    var semigroupEndo = function (_) {
        return {
            "__superclasses": {}, 
            "<>": function (_110) {
                return function (_111) {
                    return Endo(Prelude["<<<"](Prelude.semigroupoidArr({}))(_110.values[0])(_111.values[0]));
                };
            }
        };
    };
    var runEndo = function (_109) {
        return _109.values[0];
    };
    var monoidEndo = function (_) {
        return {
            "__superclasses": {
                "Prelude.Semigroup_0": function (_) {
                    return semigroupEndo({});
                }
            }, 
            mempty: Endo(Prelude.id(Prelude.categoryArr({})))
        };
    };
    return {
        Endo: Endo, 
        runEndo: runEndo, 
        semigroupEndo: semigroupEndo, 
        monoidEndo: monoidEndo
    };
})();
var PS = PS || {};
PS.Data_Monoid_First = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Maybe = PS.Data_Maybe;
    var First = function (value0) {
        return {
            ctor: "Data.Monoid.First.First", 
            values: [ value0 ]
        };
    };
    var showFirst = function (__dict_Show_36) {
        return {
            "__superclasses": {}, 
            show: function (_119) {
                return "First (" + Prelude.show(Data_Maybe.showMaybe(__dict_Show_36))(_119.values[0]) + ")";
            }
        };
    };
    var semigroupFirst = function (_) {
        return {
            "__superclasses": {}, 
            "<>": function (_120) {
                return function (_121) {
                    if ((_120.values[0]).ctor === "Data.Maybe.Just") {
                        return _120;
                    };
                    return _121;
                };
            }
        };
    };
    var runFirst = function (_112) {
        return _112.values[0];
    };
    var monoidFirst = function (_) {
        return {
            "__superclasses": {
                "Prelude.Semigroup_0": function (_) {
                    return semigroupFirst({});
                }
            }, 
            mempty: First(Data_Maybe.Nothing)
        };
    };
    var eqFirst = function (__dict_Eq_38) {
        return {
            "__superclasses": {}, 
            "==": function (_113) {
                return function (_114) {
                    return Prelude["=="](Data_Maybe.eqMaybe(__dict_Eq_38))(_113.values[0])(_114.values[0]);
                };
            }, 
            "/=": function (_115) {
                return function (_116) {
                    return Prelude["/="](Data_Maybe.eqMaybe(__dict_Eq_38))(_115.values[0])(_116.values[0]);
                };
            }
        };
    };
    var ordFirst = function (__dict_Ord_37) {
        return {
            "__superclasses": {
                "Prelude.Eq_0": function (_) {
                    return eqFirst(__dict_Ord_37["__superclasses"]["Prelude.Eq_0"]({}));
                }
            }, 
            compare: function (_117) {
                return function (_118) {
                    return Prelude.compare(Data_Maybe.ordMaybe(__dict_Ord_37))(_117.values[0])(_118.values[0]);
                };
            }
        };
    };
    return {
        First: First, 
        runFirst: runFirst, 
        eqFirst: eqFirst, 
        ordFirst: ordFirst, 
        showFirst: showFirst, 
        semigroupFirst: semigroupFirst, 
        monoidFirst: monoidFirst
    };
})();
var PS = PS || {};
PS.Data_Monoid_Last = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Maybe = PS.Data_Maybe;
    var Last = function (value0) {
        return {
            ctor: "Data.Monoid.Last.Last", 
            values: [ value0 ]
        };
    };
    var showLast = function (__dict_Show_39) {
        return {
            "__superclasses": {}, 
            show: function (_129) {
                return "Last (" + Prelude.show(Data_Maybe.showMaybe(__dict_Show_39))(_129.values[0]) + ")";
            }
        };
    };
    var semigroupLast = function (_) {
        return {
            "__superclasses": {}, 
            "<>": function (_130) {
                return function (_131) {
                    if ((_131.values[0]).ctor === "Data.Maybe.Just") {
                        return _131;
                    };
                    if ((_131.values[0]).ctor === "Data.Maybe.Nothing") {
                        return _130;
                    };
                    throw "Failed pattern match";
                };
            }
        };
    };
    var runLast = function (_122) {
        return _122.values[0];
    };
    var monoidLast = function (_) {
        return {
            "__superclasses": {
                "Prelude.Semigroup_0": function (_) {
                    return semigroupLast({});
                }
            }, 
            mempty: Last(Data_Maybe.Nothing)
        };
    };
    var eqLast = function (__dict_Eq_41) {
        return {
            "__superclasses": {}, 
            "==": function (_123) {
                return function (_124) {
                    return Prelude["=="](Data_Maybe.eqMaybe(__dict_Eq_41))(_123.values[0])(_124.values[0]);
                };
            }, 
            "/=": function (_125) {
                return function (_126) {
                    return Prelude["/="](Data_Maybe.eqMaybe(__dict_Eq_41))(_125.values[0])(_126.values[0]);
                };
            }
        };
    };
    var ordLast = function (__dict_Ord_40) {
        return {
            "__superclasses": {
                "Prelude.Eq_0": function (_) {
                    return eqLast(__dict_Ord_40["__superclasses"]["Prelude.Eq_0"]({}));
                }
            }, 
            compare: function (_127) {
                return function (_128) {
                    return Prelude.compare(Data_Maybe.ordMaybe(__dict_Ord_40))(_127.values[0])(_128.values[0]);
                };
            }
        };
    };
    return {
        Last: Last, 
        runLast: runLast, 
        eqLast: eqLast, 
        ordLast: ordLast, 
        showLast: showLast, 
        semigroupLast: semigroupLast, 
        monoidLast: monoidLast
    };
})();
var PS = PS || {};
PS.Data_Monoid_Product = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Product = function (value0) {
        return {
            ctor: "Data.Monoid.Product.Product", 
            values: [ value0 ]
        };
    };
    var showProduct = function (_) {
        return {
            "__superclasses": {}, 
            show: function (_139) {
                return "Product (" + Prelude.show(Prelude.showNumber({}))(_139.values[0]) + ")";
            }
        };
    };
    var semigroupProduct = function (_) {
        return {
            "__superclasses": {}, 
            "<>": function (_140) {
                return function (_141) {
                    return Product(_140.values[0] * _141.values[0]);
                };
            }
        };
    };
    var runProduct = function (_132) {
        return _132.values[0];
    };
    var monoidProduct = function (_) {
        return {
            "__superclasses": {
                "Prelude.Semigroup_0": function (_) {
                    return semigroupProduct({});
                }
            }, 
            mempty: Product(1)
        };
    };
    var eqProduct = function (_) {
        return {
            "__superclasses": {}, 
            "==": function (_133) {
                return function (_134) {
                    return _133.values[0] === _134.values[0];
                };
            }, 
            "/=": function (_135) {
                return function (_136) {
                    return _135.values[0] !== _136.values[0];
                };
            }
        };
    };
    var ordProduct = function (_) {
        return {
            "__superclasses": {
                "Prelude.Eq_0": function (_) {
                    return eqProduct({});
                }
            }, 
            compare: function (_137) {
                return function (_138) {
                    return Prelude.compare(Prelude.ordNumber({}))(_137.values[0])(_138.values[0]);
                };
            }
        };
    };
    return {
        Product: Product, 
        runProduct: runProduct, 
        eqProduct: eqProduct, 
        ordProduct: ordProduct, 
        showProduct: showProduct, 
        semigroupProduct: semigroupProduct, 
        monoidProduct: monoidProduct
    };
})();
var PS = PS || {};
PS.Data_Monoid_Sum = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Sum = function (value0) {
        return {
            ctor: "Data.Monoid.Sum.Sum", 
            values: [ value0 ]
        };
    };
    var showSum = function (_) {
        return {
            "__superclasses": {}, 
            show: function (_149) {
                return "Sum (" + Prelude.show(Prelude.showNumber({}))(_149.values[0]) + ")";
            }
        };
    };
    var semigroupSum = function (_) {
        return {
            "__superclasses": {}, 
            "<>": function (_150) {
                return function (_151) {
                    return Sum(_150.values[0] + _151.values[0]);
                };
            }
        };
    };
    var runSum = function (_142) {
        return _142.values[0];
    };
    var monoidSum = function (_) {
        return {
            "__superclasses": {
                "Prelude.Semigroup_0": function (_) {
                    return semigroupSum({});
                }
            }, 
            mempty: Sum(0)
        };
    };
    var eqSum = function (_) {
        return {
            "__superclasses": {}, 
            "==": function (_143) {
                return function (_144) {
                    return _143.values[0] === _144.values[0];
                };
            }, 
            "/=": function (_145) {
                return function (_146) {
                    return _145.values[0] !== _146.values[0];
                };
            }
        };
    };
    var ordSum = function (_) {
        return {
            "__superclasses": {
                "Prelude.Eq_0": function (_) {
                    return eqSum({});
                }
            }, 
            compare: function (_147) {
                return function (_148) {
                    return Prelude.compare(Prelude.ordNumber({}))(_147.values[0])(_148.values[0]);
                };
            }
        };
    };
    return {
        Sum: Sum, 
        runSum: runSum, 
        eqSum: eqSum, 
        ordSum: ordSum, 
        showSum: showSum, 
        semigroupSum: semigroupSum, 
        monoidSum: monoidSum
    };
})();
var PS = PS || {};
PS.Data_Tuple = (function () {
    "use strict";
    var Data_Array = PS.Data_Array;
    var Prelude = PS.Prelude;
    var Data_Monoid = PS.Data_Monoid;
    var Tuple = function (value0) {
        return function (value1) {
            return {
                ctor: "Data.Tuple.Tuple", 
                values: [ value0, value1 ]
            };
        };
    };
    var zip = Data_Array.zipWith(Tuple);
    var unzip = function (_156) {
        if (_156.length > 0) {
            var _530 = _156.slice(1);
            return (function (_526) {
                return Tuple(Prelude[":"]((_156[0]).values[0])(_526.values[0]))(Prelude[":"]((_156[0]).values[1])(_526.values[1]));
            })(unzip(_530));
        };
        if (_156.length === 0) {
            return Tuple([  ])([  ]);
        };
        throw "Failed pattern match";
    };
    var uncurry = function (_154) {
        return function (_155) {
            return _154(_155.values[0])(_155.values[1]);
        };
    };
    var swap = function (_157) {
        return Tuple(_157.values[1])(_157.values[0]);
    };
    var snd = function (_153) {
        return _153.values[1];
    };
    var showTuple = function (__dict_Show_42) {
        return function (__dict_Show_43) {
            return {
                "__superclasses": {}, 
                show: function (_158) {
                    return "Tuple(" + Prelude.show(__dict_Show_42)(_158.values[0]) + ", " + Prelude.show(__dict_Show_43)(_158.values[1]) + ")";
                }
            };
        };
    };
    var functorTuple = function (_) {
        return {
            "__superclasses": {}, 
            "<$>": function (_163) {
                return function (_164) {
                    return Tuple(_164.values[0])(_163(_164.values[1]));
                };
            }
        };
    };
    var fst = function (_152) {
        return _152.values[0];
    };
    var eqTuple = function (__dict_Eq_47) {
        return function (__dict_Eq_48) {
            return {
                "__superclasses": {}, 
                "==": function (_159) {
                    return function (_160) {
                        return Prelude["=="](__dict_Eq_47)(_159.values[0])(_160.values[0]) && Prelude["=="](__dict_Eq_48)(_159.values[1])(_160.values[1]);
                    };
                }, 
                "/=": function (t1) {
                    return function (t2) {
                        return !Prelude["=="](eqTuple(__dict_Eq_47)(__dict_Eq_48))(t1)(t2);
                    };
                }
            };
        };
    };
    var ordTuple = function (__dict_Ord_44) {
        return function (__dict_Ord_45) {
            return {
                "__superclasses": {
                    "Prelude.Eq_0": function (_) {
                        return eqTuple(__dict_Ord_44["__superclasses"]["Prelude.Eq_0"]({}))(__dict_Ord_45["__superclasses"]["Prelude.Eq_0"]({}));
                    }
                }, 
                compare: function (_161) {
                    return function (_162) {
                        return (function (_561) {
                            if (_561.ctor === "Prelude.EQ") {
                                return Prelude.compare(__dict_Ord_45)(_161.values[1])(_162.values[1]);
                            };
                            return _561;
                        })(Prelude.compare(__dict_Ord_44)(_161.values[0])(_162.values[0]));
                    };
                }
            };
        };
    };
    var curry = function (f) {
        return function (a) {
            return function (b) {
                return f(Tuple(a)(b));
            };
        };
    };
    var applyTuple = function (__dict_Semigroup_50) {
        return {
            "__superclasses": {
                "Prelude.Functor_0": function (_) {
                    return functorTuple({});
                }
            }, 
            "<*>": function (_165) {
                return function (_166) {
                    return Tuple(Prelude["<>"](__dict_Semigroup_50)(_165.values[0])(_166.values[0]))(_165.values[1](_166.values[1]));
                };
            }
        };
    };
    var bindTuple = function (__dict_Semigroup_49) {
        return {
            "__superclasses": {
                "Prelude.Apply_0": function (_) {
                    return applyTuple(__dict_Semigroup_49);
                }
            }, 
            ">>=": function (_167) {
                return function (_168) {
                    return (function (_574) {
                        return Tuple(Prelude["<>"](__dict_Semigroup_49)(_167.values[0])(_574.values[0]))(_574.values[1]);
                    })(_168(_167.values[1]));
                };
            }
        };
    };
    var applicativeTuple = function (__dict_Monoid_51) {
        return {
            "__superclasses": {
                "Prelude.Apply_0": function (_) {
                    return applyTuple(__dict_Monoid_51["__superclasses"]["Prelude.Semigroup_0"]({}));
                }
            }, 
            pure: Tuple(Data_Monoid.mempty(__dict_Monoid_51))
        };
    };
    var monadTuple = function (__dict_Monoid_46) {
        return {
            "__superclasses": {
                "Prelude.Applicative_0": function (_) {
                    return applicativeTuple(__dict_Monoid_46);
                }, 
                "Prelude.Bind_1": function (_) {
                    return bindTuple(__dict_Monoid_46["__superclasses"]["Prelude.Semigroup_0"]({}));
                }
            }
        };
    };
    return {
        Tuple: Tuple, 
        swap: swap, 
        unzip: unzip, 
        zip: zip, 
        uncurry: uncurry, 
        curry: curry, 
        snd: snd, 
        fst: fst, 
        showTuple: showTuple, 
        eqTuple: eqTuple, 
        ordTuple: ordTuple, 
        functorTuple: functorTuple, 
        applyTuple: applyTuple, 
        applicativeTuple: applicativeTuple, 
        bindTuple: bindTuple, 
        monadTuple: monadTuple
    };
})();
var PS = PS || {};
PS.Golly = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Maybe = PS.Data_Maybe;
    var Data_Array = PS.Data_Array;
    var AllocSegment = function (value0) {
        return function (value1) {
            return {
                ctor: "Golly.AllocSegment", 
                values: [ value0, value1 ]
            };
        };
    };
    var Alloc = function (value0) {
        return function (value1) {
            return {
                ctor: "Golly.Alloc", 
                values: [ value0, value1 ]
            };
        };
    };
    var Grouping = function (value0) {
        return function (value1) {
            return {
                ctor: "Golly.Grouping", 
                values: [ value0, value1 ]
            };
        };
    };
    var sum = function (_169) {
        if (_169.length === 0) {
            return 0;
        };
        if (_169.length > 0) {
            var _581 = _169.slice(1);
            return _169[0] + sum(_581);
        };
        throw "Failed pattern match";
    };
    var segName = function (_170) {
        return _170.values[0];
    };
    var sampleData = [ Grouping("Priority")([ Alloc(Data_Maybe.Nothing)(57), Alloc(Data_Maybe.Just(AllocSegment("High")(1)))(20), Alloc(Data_Maybe.Just(AllocSegment("Medium")(2)))(17), Alloc(Data_Maybe.Just(AllocSegment("Low")(3)))(4) ]), Grouping("Version")([ Alloc(Data_Maybe.Nothing)((((57 + 20) + 17) + 4 - 63) + 1), Alloc(Data_Maybe.Just(AllocSegment("v2.x")(20)))(60), Alloc(Data_Maybe.Just(AllocSegment("v3.x")(21)))(0), Alloc(Data_Maybe.Just(AllocSegment("Defer")(22)))(3) ]) ];
    var isUnalloc = function (_172) {
        if ((_172.values[0]).ctor === "Data.Maybe.Nothing") {
            return true;
        };
        if ((_172.values[0]).ctor === "Data.Maybe.Just") {
            return false;
        };
        throw "Failed pattern match";
    };
    var allocQty = function (_171) {
        return _171.values[1];
    };
    var countUnAp = function (_173) {
        return sum(Data_Array.map(allocQty)(Data_Array.filter(isUnalloc)(_173.values[1])));
    };
    return {
        Grouping: Grouping, 
        Alloc: Alloc, 
        AllocSegment: AllocSegment, 
        countUnAp: countUnAp, 
        isUnalloc: isUnalloc, 
        allocQty: allocQty, 
        segName: segName, 
        sampleData: sampleData, 
        sum: sum
    };
})();
var PS = PS || {};
PS.Control_Monad_Eff = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function returnE(a) {  return function() {    return a;  };};
    function bindE(a) {  return function(f) {    return function() {      return f(a())();    };  };};
    function runPure(f) {  return f();};
    function untilE(f) {  return function() {    while (!f()) { }    return {};  };};
    function whileE(f) {  return function(a) {    return function() {      while (f()) {        a();      }      return {};    };  };};
    function forE(lo) {  return function(hi) {    return function(f) {      return function() {        for (var i = lo; i < hi; i++) {          f(i)();        }      };    };  };};
    function foreachE(as) {  return function(f) {    for (var i = 0; i < as.length; i++) {      f(as[i])();    }  };};
    var applicativeEff = function (_) {
        return {
            "__superclasses": {
                "Prelude.Apply_0": function (_) {
                    return applyEff({});
                }
            }, 
            pure: returnE
        };
    };
    var applyEff = function (_) {
        return {
            "__superclasses": {
                "Prelude.Functor_0": function (_) {
                    return functorEff({});
                }
            }, 
            "<*>": Prelude.ap(monadEff({}))
        };
    };
    var functorEff = function (_) {
        return {
            "__superclasses": {}, 
            "<$>": Prelude.liftA1(applicativeEff({}))
        };
    };
    var monadEff = function (_) {
        return {
            "__superclasses": {
                "Prelude.Applicative_0": function (_) {
                    return applicativeEff({});
                }, 
                "Prelude.Bind_1": function (_) {
                    return bindEff({});
                }
            }
        };
    };
    var bindEff = function (_) {
        return {
            "__superclasses": {
                "Prelude.Apply_0": function (_) {
                    return applyEff({});
                }
            }, 
            ">>=": bindE
        };
    };
    return {
        foreachE: foreachE, 
        forE: forE, 
        whileE: whileE, 
        untilE: untilE, 
        runPure: runPure, 
        bindE: bindE, 
        returnE: returnE, 
        functorEff: functorEff, 
        applyEff: applyEff, 
        applicativeEff: applicativeEff, 
        bindEff: bindEff, 
        monadEff: monadEff
    };
})();
var PS = PS || {};
PS.Control_Monad_Eff_Unsafe = (function () {
    "use strict";
    function unsafeInterleaveEff(f) {  return f;};
    return {
        unsafeInterleaveEff: unsafeInterleaveEff
    };
})();
var PS = PS || {};
PS.Control_Monad_ST = (function () {
    "use strict";
    function newSTRef(val) {  return function () {    return { value: val };  };};
    function readSTRef(ref) {  return function() {    return ref.value;  };};
    function modifySTRef(ref) {  return function(f) {    return function() {      return ref.value = f(ref.value);    };  };};
    function writeSTRef(ref) {  return function(a) {    return function() {      return ref.value = a;    };  };};
    function newSTArray(len) {  return function(a) {    return function() {      var arr = [];      for (var i = 0; i < len; i++) {        arr[i] = a;      };      return arr;    };  };};
    function peekSTArray(arr) {  return function(i) {    return function() {      return arr[i];    };  };};
    function pokeSTArray(arr) {  return function(i) {    return function(a) {      return function() {        return arr[i] = a;      };    };  };};
    function runST(f) {  return f;};
    function runSTArray(f) {  return f;};
    return {
        runSTArray: runSTArray, 
        runST: runST, 
        pokeSTArray: pokeSTArray, 
        peekSTArray: peekSTArray, 
        newSTArray: newSTArray, 
        writeSTRef: writeSTRef, 
        modifySTRef: modifySTRef, 
        readSTRef: readSTRef, 
        newSTRef: newSTRef
    };
})();
var PS = PS || {};
PS.Debug_Trace = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    function trace(s) {  return function() {    console.log(s);    return {};  };};
    var print = function (__dict_Show_52) {
        return function (o) {
            return trace(Prelude.show(__dict_Show_52)(o));
        };
    };
    return {
        print: print, 
        trace: trace
    };
})();
var PS = PS || {};
PS.Control_Monad = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var when = function (__dict_Monad_53) {
        return function (_179) {
            return function (_180) {
                if (_179) {
                    return _180;
                };
                if (!_179) {
                    return Prelude["return"](__dict_Monad_53)({});
                };
                throw "Failed pattern match";
            };
        };
    };
    var unless = function (__dict_Monad_54) {
        return function (_181) {
            return function (_182) {
                if (!_181) {
                    return _182;
                };
                if (_181) {
                    return Prelude["return"](__dict_Monad_54)({});
                };
                throw "Failed pattern match";
            };
        };
    };
    var replicateM = function (__dict_Monad_55) {
        return function (_174) {
            return function (_175) {
                if (_174 === 0) {
                    return Prelude["return"](__dict_Monad_55)([  ]);
                };
                return Prelude[">>="](__dict_Monad_55["__superclasses"]["Prelude.Bind_1"]({}))(_175)(function (_4) {
                    return Prelude[">>="](__dict_Monad_55["__superclasses"]["Prelude.Bind_1"]({}))(replicateM(__dict_Monad_55)(_174 - 1)(_175))(function (_3) {
                        return Prelude["return"](__dict_Monad_55)(Prelude[":"](_4)(_3));
                    });
                });
            };
        };
    };
    var foldM = function (__dict_Monad_56) {
        return function (_176) {
            return function (_177) {
                return function (_178) {
                    if (_178.length === 0) {
                        return Prelude["return"](__dict_Monad_56)(_177);
                    };
                    if (_178.length > 0) {
                        var _609 = _178.slice(1);
                        return Prelude[">>="](__dict_Monad_56["__superclasses"]["Prelude.Bind_1"]({}))(_176(_177)(_178[0]))(function (a$prime) {
                            return foldM(__dict_Monad_56)(_176)(a$prime)(_609);
                        });
                    };
                    throw "Failed pattern match";
                };
            };
        };
    };
    return {
        unless: unless, 
        when: when, 
        foldM: foldM, 
        replicateM: replicateM
    };
})();
var PS = PS || {};
PS.Control_Bind = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var $greater$eq$greater = function (__dict_Bind_57) {
        return function (f) {
            return function (g) {
                return function (a) {
                    return Prelude[">>="](__dict_Bind_57)(f(a))(g);
                };
            };
        };
    };
    var $eq$less$less = function (__dict_Bind_58) {
        return function (f) {
            return function (m) {
                return Prelude[">>="](__dict_Bind_58)(m)(f);
            };
        };
    };
    var $less$eq$less = function (__dict_Bind_59) {
        return function (f) {
            return function (g) {
                return function (a) {
                    return $eq$less$less(__dict_Bind_59)(f)(g(a));
                };
            };
        };
    };
    var join = function (__dict_Bind_60) {
        return function (m) {
            return Prelude[">>="](__dict_Bind_60)(m)(Prelude.id(Prelude.categoryArr({})));
        };
    };
    var ifM = function (__dict_Bind_61) {
        return function (cond) {
            return function (t) {
                return function (f) {
                    return Prelude[">>="](__dict_Bind_61)(cond)(function (cond$prime) {
                        return cond$prime ? t : f;
                    });
                };
            };
        };
    };
    return {
        ifM: ifM, 
        join: join, 
        "<=<": $less$eq$less, 
        ">=>": $greater$eq$greater, 
        "=<<": $eq$less$less
    };
})();
var PS = PS || {};
PS.Control_Apply = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var $less$times = function (__dict_Apply_62) {
        return function (a) {
            return function (b) {
                return Prelude["<*>"](__dict_Apply_62)(Prelude["<$>"](__dict_Apply_62["__superclasses"]["Prelude.Functor_0"]({}))(Prelude["const"])(a))(b);
            };
        };
    };
    var $times$greater = function (__dict_Apply_63) {
        return function (a) {
            return function (b) {
                return Prelude["<*>"](__dict_Apply_63)(Prelude["<$>"](__dict_Apply_63["__superclasses"]["Prelude.Functor_0"]({}))(Prelude["const"](Prelude.id(Prelude.categoryArr({}))))(a))(b);
            };
        };
    };
    var lift5 = function (__dict_Apply_64) {
        return function (f) {
            return function (a) {
                return function (b) {
                    return function (c) {
                        return function (d) {
                            return function (e) {
                                return Prelude["<*>"](__dict_Apply_64)(Prelude["<*>"](__dict_Apply_64)(Prelude["<*>"](__dict_Apply_64)(Prelude["<*>"](__dict_Apply_64)(Prelude["<$>"](__dict_Apply_64["__superclasses"]["Prelude.Functor_0"]({}))(f)(a))(b))(c))(d))(e);
                            };
                        };
                    };
                };
            };
        };
    };
    var lift4 = function (__dict_Apply_65) {
        return function (f) {
            return function (a) {
                return function (b) {
                    return function (c) {
                        return function (d) {
                            return Prelude["<*>"](__dict_Apply_65)(Prelude["<*>"](__dict_Apply_65)(Prelude["<*>"](__dict_Apply_65)(Prelude["<$>"](__dict_Apply_65["__superclasses"]["Prelude.Functor_0"]({}))(f)(a))(b))(c))(d);
                        };
                    };
                };
            };
        };
    };
    var lift3 = function (__dict_Apply_66) {
        return function (f) {
            return function (a) {
                return function (b) {
                    return function (c) {
                        return Prelude["<*>"](__dict_Apply_66)(Prelude["<*>"](__dict_Apply_66)(Prelude["<$>"](__dict_Apply_66["__superclasses"]["Prelude.Functor_0"]({}))(f)(a))(b))(c);
                    };
                };
            };
        };
    };
    var lift2 = function (__dict_Apply_67) {
        return function (f) {
            return function (a) {
                return function (b) {
                    return Prelude["<*>"](__dict_Apply_67)(Prelude["<$>"](__dict_Apply_67["__superclasses"]["Prelude.Functor_0"]({}))(f)(a))(b);
                };
            };
        };
    };
    var forever = function (__dict_Apply_68) {
        return function (a) {
            return $times$greater(__dict_Apply_68)(a)(forever(__dict_Apply_68)(a));
        };
    };
    return {
        forever: forever, 
        lift5: lift5, 
        lift4: lift4, 
        lift3: lift3, 
        lift2: lift2, 
        "*>": $times$greater, 
        "<*": $less$times
    };
})();
var PS = PS || {};
PS.Data_Foldable = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Control_Apply = PS.Control_Apply;
    var Data_Monoid = PS.Data_Monoid;
    var Data_Monoid_First = PS.Data_Monoid_First;
    var Data_Maybe = PS.Data_Maybe;
    function foldrArray(f) {  return function(z) {    return function(xs) {      var acc = z;      for (var i = xs.length - 1; i >= 0; --i) {        acc = f(xs[i])(acc);      }      return acc;    }  }};
    function foldlArray(f) {  return function(z) {    return function(xs) {      var acc = z;      for (var i = 0, len = xs.length; i < len; ++i) {        acc = f(acc)(xs[i]);      }      return acc;    }  }};
    var foldr = function (dict) {
        return dict.foldr;
    };
    var traverse_ = function (__dict_Functor_69) {
        return function (__dict_Applicative_70) {
            return function (__dict_Foldable_71) {
                return function (f) {
                    return foldr(__dict_Foldable_71)(Prelude["<<<"](Prelude.semigroupoidArr({}))(Control_Apply["*>"](__dict_Applicative_70["__superclasses"]["Prelude.Apply_0"]({})))(f))(Prelude.pure(__dict_Applicative_70)({}));
                };
            };
        };
    };
    var for_ = function (__dict_Functor_72) {
        return function (__dict_Applicative_73) {
            return function (__dict_Foldable_74) {
                return Prelude.flip(traverse_(__dict_Functor_72)(__dict_Applicative_73)(__dict_Foldable_74));
            };
        };
    };
    var sequence_ = function (__dict_Functor_75) {
        return function (__dict_Applicative_76) {
            return function (__dict_Foldable_77) {
                return traverse_(__dict_Functor_75)(__dict_Applicative_76)(__dict_Foldable_77)(Prelude.id(Prelude.categoryArr({})));
            };
        };
    };
    var foldl = function (dict) {
        return dict.foldl;
    };
    var mconcat = function (__dict_Foldable_78) {
        return function (__dict_Monoid_79) {
            return foldl(__dict_Foldable_78)(Prelude["<>"](__dict_Monoid_79["__superclasses"]["Prelude.Semigroup_0"]({})))(Data_Monoid.mempty(__dict_Monoid_79));
        };
    };
    var or = function (__dict_Foldable_80) {
        return foldl(__dict_Foldable_80)(Prelude["||"](Prelude.boolLikeBoolean({})))(false);
    };
    var product = function (__dict_Foldable_81) {
        return foldl(__dict_Foldable_81)(Prelude["*"](Prelude.numNumber({})))(1);
    };
    var sum = function (__dict_Foldable_82) {
        return foldl(__dict_Foldable_82)(Prelude["+"](Prelude.numNumber({})))(0);
    };
    var foldableTuple = function (_) {
        return {
            "__superclasses": {}, 
            foldr: function (_208) {
                return function (_209) {
                    return function (_210) {
                        return _208(_210.values[1])(_209);
                    };
                };
            }, 
            foldl: function (_211) {
                return function (_212) {
                    return function (_213) {
                        return _211(_212)(_213.values[1]);
                    };
                };
            }, 
            foldMap: function (__dict_Monoid_83) {
                return function (_214) {
                    return function (_215) {
                        return _214(_215.values[1]);
                    };
                };
            }
        };
    };
    var foldableRef = function (_) {
        return {
            "__superclasses": {}, 
            foldr: function (_200) {
                return function (_201) {
                    return function (_202) {
                        return _200(_202.values[0])(_201);
                    };
                };
            }, 
            foldl: function (_203) {
                return function (_204) {
                    return function (_205) {
                        return _203(_204)(_205.values[0]);
                    };
                };
            }, 
            foldMap: function (__dict_Monoid_84) {
                return function (_206) {
                    return function (_207) {
                        return _206(_207.values[0]);
                    };
                };
            }
        };
    };
    var foldableMaybe = function (_) {
        return {
            "__superclasses": {}, 
            foldr: function (_192) {
                return function (_193) {
                    return function (_194) {
                        if (_194.ctor === "Data.Maybe.Nothing") {
                            return _193;
                        };
                        if (_194.ctor === "Data.Maybe.Just") {
                            return _192(_194.values[0])(_193);
                        };
                        throw "Failed pattern match";
                    };
                };
            }, 
            foldl: function (_195) {
                return function (_196) {
                    return function (_197) {
                        if (_197.ctor === "Data.Maybe.Nothing") {
                            return _196;
                        };
                        if (_197.ctor === "Data.Maybe.Just") {
                            return _195(_196)(_197.values[0]);
                        };
                        throw "Failed pattern match";
                    };
                };
            }, 
            foldMap: function (__dict_Monoid_85) {
                return function (_198) {
                    return function (_199) {
                        if (_199.ctor === "Data.Maybe.Nothing") {
                            return Data_Monoid.mempty(__dict_Monoid_85);
                        };
                        if (_199.ctor === "Data.Maybe.Just") {
                            return _198(_199.values[0]);
                        };
                        throw "Failed pattern match";
                    };
                };
            }
        };
    };
    var foldableEither = function (_) {
        return {
            "__superclasses": {}, 
            foldr: function (_184) {
                return function (_185) {
                    return function (_186) {
                        if (_186.ctor === "Data.Either.Left") {
                            return _185;
                        };
                        if (_186.ctor === "Data.Either.Right") {
                            return _184(_186.values[0])(_185);
                        };
                        throw "Failed pattern match";
                    };
                };
            }, 
            foldl: function (_187) {
                return function (_188) {
                    return function (_189) {
                        if (_189.ctor === "Data.Either.Left") {
                            return _188;
                        };
                        if (_189.ctor === "Data.Either.Right") {
                            return _187(_188)(_189.values[0]);
                        };
                        throw "Failed pattern match";
                    };
                };
            }, 
            foldMap: function (__dict_Monoid_86) {
                return function (_190) {
                    return function (_191) {
                        if (_191.ctor === "Data.Either.Left") {
                            return Data_Monoid.mempty(__dict_Monoid_86);
                        };
                        if (_191.ctor === "Data.Either.Right") {
                            return _190(_191.values[0]);
                        };
                        throw "Failed pattern match";
                    };
                };
            }
        };
    };
    var foldableArray = function (_) {
        return {
            "__superclasses": {}, 
            foldr: function (f) {
                return function (z) {
                    return function (xs) {
                        return foldrArray(f)(z)(xs);
                    };
                };
            }, 
            foldl: function (f) {
                return function (z) {
                    return function (xs) {
                        return foldlArray(f)(z)(xs);
                    };
                };
            }, 
            foldMap: function (__dict_Monoid_87) {
                return function (f) {
                    return function (xs) {
                        return foldr(foldableArray({}))(function (x) {
                            return function (acc) {
                                return Prelude["<>"](__dict_Monoid_87["__superclasses"]["Prelude.Semigroup_0"]({}))(f(x))(acc);
                            };
                        })(Data_Monoid.mempty(__dict_Monoid_87))(xs);
                    };
                };
            }
        };
    };
    var foldMap = function (dict) {
        return dict.foldMap;
    };
    var lookup = function (__dict_Eq_88) {
        return function (__dict_Foldable_89) {
            return function (a) {
                return function (f) {
                    return Data_Monoid_First.runFirst(foldMap(__dict_Foldable_89)(Data_Monoid_First.monoidFirst({}))(function (_183) {
                        return Data_Monoid_First.First(Prelude["=="](__dict_Eq_88)(a)(_183.values[0]) ? Data_Maybe.Just(_183.values[1]) : Data_Maybe.Nothing);
                    })(f));
                };
            };
        };
    };
    var fold = function (__dict_Foldable_90) {
        return function (__dict_Monoid_91) {
            return foldMap(__dict_Foldable_90)(__dict_Monoid_91)(Prelude.id(Prelude.categoryArr({})));
        };
    };
    var find = function (__dict_Foldable_92) {
        return function (p) {
            return function (f) {
                return (function (_663) {
                    if (_663.length > 0) {
                        return Data_Maybe.Just(_663[0]);
                    };
                    if (_663.length === 0) {
                        return Data_Maybe.Nothing;
                    };
                    throw "Failed pattern match";
                })(foldMap(__dict_Foldable_92)(Data_Monoid.monoidArray({}))(function (x) {
                    return p(x) ? [ x ] : [  ];
                })(f));
            };
        };
    };
    var any = function (__dict_Foldable_93) {
        return function (p) {
            return Prelude["<<<"](Prelude.semigroupoidArr({}))(or(foldableArray({})))(foldMap(__dict_Foldable_93)(Data_Monoid.monoidArray({}))(function (x) {
                return [ p(x) ];
            }));
        };
    };
    var elem = function (__dict_Eq_94) {
        return function (__dict_Foldable_95) {
            return Prelude["<<<"](Prelude.semigroupoidArr({}))(any(__dict_Foldable_95))(Prelude["=="](__dict_Eq_94));
        };
    };
    var notElem = function (__dict_Eq_96) {
        return function (__dict_Foldable_97) {
            return function (x) {
                return Prelude["<<<"](Prelude.semigroupoidArr({}))(Prelude.not(Prelude.boolLikeBoolean({})))(elem(__dict_Eq_96)(__dict_Foldable_97)(x));
            };
        };
    };
    var and = function (__dict_Foldable_98) {
        return foldl(__dict_Foldable_98)(Prelude["&&"](Prelude.boolLikeBoolean({})))(true);
    };
    var all = function (__dict_Foldable_99) {
        return function (p) {
            return Prelude["<<<"](Prelude.semigroupoidArr({}))(and(foldableArray({})))(foldMap(__dict_Foldable_99)(Data_Monoid.monoidArray({}))(function (x) {
                return [ p(x) ];
            }));
        };
    };
    return {
        foldlArray: foldlArray, 
        foldrArray: foldrArray, 
        lookup: lookup, 
        find: find, 
        notElem: notElem, 
        elem: elem, 
        product: product, 
        sum: sum, 
        all: all, 
        any: any, 
        or: or, 
        and: and, 
        mconcat: mconcat, 
        "sequence_": sequence_, 
        "for_": for_, 
        "traverse_": traverse_, 
        fold: fold, 
        foldMap: foldMap, 
        foldl: foldl, 
        foldr: foldr, 
        foldableArray: foldableArray, 
        foldableEither: foldableEither, 
        foldableMaybe: foldableMaybe, 
        foldableRef: foldableRef, 
        foldableTuple: foldableTuple
    };
})();
var PS = PS || {};
PS.Data_Traversable = (function () {
    "use strict";
    var Prelude = PS.Prelude;
    var Data_Tuple = PS.Data_Tuple;
    var Data_Eq = PS.Data_Eq;
    var Data_Maybe = PS.Data_Maybe;
    var Data_Either = PS.Data_Either;
    var Data_Array = PS.Data_Array;
    var traverse = function (dict) {
        return dict.traverse;
    };
    var traversableTuple = function (_) {
        return {
            "__superclasses": {}, 
            traverse: function (__dict_Functor_100) {
                return function (__dict_Applicative_101) {
                    return function (_228) {
                        return function (_229) {
                            return Prelude["<$>"](__dict_Functor_100)(Data_Tuple.Tuple(_229.values[0]))(_228(_229.values[1]));
                        };
                    };
                };
            }, 
            sequence: function (__dict_Functor_102) {
                return function (__dict_Applicative_103) {
                    return function (_230) {
                        return Prelude["<$>"](__dict_Functor_102)(Data_Tuple.Tuple(_230.values[0]))(_230.values[1]);
                    };
                };
            }
        };
    };
    var traversableRef = function (_) {
        return {
            "__superclasses": {}, 
            traverse: function (__dict_Functor_104) {
                return function (__dict_Applicative_105) {
                    return function (_222) {
                        return function (_223) {
                            return Prelude["<$>"](__dict_Functor_104)(Data_Eq.Ref)(_222(_223.values[0]));
                        };
                    };
                };
            }, 
            sequence: function (__dict_Functor_106) {
                return function (__dict_Applicative_107) {
                    return function (_224) {
                        return Prelude["<$>"](__dict_Functor_106)(Data_Eq.Ref)(_224.values[0]);
                    };
                };
            }
        };
    };
    var traversableMaybe = function (_) {
        return {
            "__superclasses": {}, 
            traverse: function (__dict_Functor_108) {
                return function (__dict_Applicative_109) {
                    return function (_225) {
                        return function (_226) {
                            if (_226.ctor === "Data.Maybe.Nothing") {
                                return Prelude.pure(__dict_Applicative_109)(Data_Maybe.Nothing);
                            };
                            if (_226.ctor === "Data.Maybe.Just") {
                                return Prelude["<$>"](__dict_Functor_108)(Data_Maybe.Just)(_225(_226.values[0]));
                            };
                            throw "Failed pattern match";
                        };
                    };
                };
            }, 
            sequence: function (__dict_Functor_110) {
                return function (__dict_Applicative_111) {
                    return function (_227) {
                        if (_227.ctor === "Data.Maybe.Nothing") {
                            return Prelude.pure(__dict_Applicative_111)(Data_Maybe.Nothing);
                        };
                        if (_227.ctor === "Data.Maybe.Just") {
                            return Prelude["<$>"](__dict_Functor_110)(Data_Maybe.Just)(_227.values[0]);
                        };
                        throw "Failed pattern match";
                    };
                };
            }
        };
    };
    var traversableEither = function (_) {
        return {
            "__superclasses": {}, 
            traverse: function (__dict_Functor_112) {
                return function (__dict_Applicative_113) {
                    return function (_219) {
                        return function (_220) {
                            if (_220.ctor === "Data.Either.Left") {
                                return Prelude.pure(__dict_Applicative_113)(Data_Either.Left(_220.values[0]));
                            };
                            if (_220.ctor === "Data.Either.Right") {
                                return Prelude["<$>"](__dict_Functor_112)(Data_Either.Right)(_219(_220.values[0]));
                            };
                            throw "Failed pattern match";
                        };
                    };
                };
            }, 
            sequence: function (__dict_Functor_114) {
                return function (__dict_Applicative_115) {
                    return function (_221) {
                        if (_221.ctor === "Data.Either.Left") {
                            return Prelude.pure(__dict_Applicative_115)(Data_Either.Left(_221.values[0]));
                        };
                        if (_221.ctor === "Data.Either.Right") {
                            return Prelude["<$>"](__dict_Functor_114)(Data_Either.Right)(_221.values[0]);
                        };
                        throw "Failed pattern match";
                    };
                };
            }
        };
    };
    var sequence = function (dict) {
        return dict.sequence;
    };
    var traversableArray = function (_) {
        return {
            "__superclasses": {}, 
            traverse: function (__dict_Functor_116) {
                return function (__dict_Applicative_117) {
                    return function (_216) {
                        return function (_217) {
                            if (_217.length === 0) {
                                return Prelude.pure(__dict_Applicative_117)([  ]);
                            };
                            if (_217.length > 0) {
                                var _693 = _217.slice(1);
                                return Prelude["<*>"](__dict_Applicative_117["__superclasses"]["Prelude.Apply_0"]({}))(Prelude["<$>"](__dict_Functor_116)(Prelude[":"])(_216(_217[0])))(traverse(traversableArray({}))(__dict_Functor_116)(__dict_Applicative_117)(_216)(_693));
                            };
                            throw "Failed pattern match";
                        };
                    };
                };
            }, 
            sequence: function (__dict_Functor_118) {
                return function (__dict_Applicative_119) {
                    return function (_218) {
                        if (_218.length === 0) {
                            return Prelude.pure(__dict_Applicative_119)([  ]);
                        };
                        if (_218.length > 0) {
                            var _696 = _218.slice(1);
                            return Prelude["<*>"](__dict_Applicative_119["__superclasses"]["Prelude.Apply_0"]({}))(Prelude["<$>"](__dict_Functor_118)(Prelude[":"])(_218[0]))(sequence(traversableArray({}))(__dict_Functor_118)(__dict_Applicative_119)(_696));
                        };
                        throw "Failed pattern match";
                    };
                };
            }
        };
    };
    var zipWithA = function (__dict_Functor_120) {
        return function (__dict_Applicative_121) {
            return function (f) {
                return function (xs) {
                    return function (ys) {
                        return sequence(traversableArray({}))(__dict_Functor_120)(__dict_Applicative_121)(Data_Array.zipWith(f)(xs)(ys));
                    };
                };
            };
        };
    };
    var $$for = function (__dict_Functor_122) {
        return function (__dict_Applicative_123) {
            return function (__dict_Traversable_124) {
                return function (x) {
                    return function (f) {
                        return traverse(__dict_Traversable_124)(__dict_Functor_122)(__dict_Applicative_123)(f)(x);
                    };
                };
            };
        };
    };
    return {
        zipWithA: zipWithA, 
        "for": $$for, 
        sequence: sequence, 
        traverse: traverse, 
        traversableArray: traversableArray, 
        traversableEither: traversableEither, 
        traversableRef: traversableRef, 
        traversableMaybe: traversableMaybe, 
        traversableTuple: traversableTuple
    };
})();