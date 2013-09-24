// JQuery is required separately.
// - 1) It's 93KB, separation allows for me parallelism.
// - 2) Will probably switch to CDN later.

// require "vendor/jquery-ui.js"
// require "vendor/jquery-autosize.js"
// require "vendor/jquery-timeago.js"
// require "vendor/jquery-serializeObject.js"
// require "vendor/jquery-livequery.js"
// require "vendor/jquery-rangyinputs.js"
// require "vendor/mousetrap.js"
// require "vendor/mousetrap-global-bind.js"

// =====================================================================================================================
// Functional JS
// http://dailyjs.com/2012/09/14/functional-programming/

Function.prototype.p = function() {
    var args = Array.prototype.slice.call(arguments);
    var f = this;
    return function() {
        var inner_args = Array.prototype.slice.call(arguments);
        return f.apply(this, args.concat(inner_args))
    };
};

Function.prototype.c = function(g) {
    var f = this;
    return function() {
        var args = Array.prototype.slice.call(arguments);
        return f.call(this, g.apply(this, args));
    };
};

Function.prototype.f = function() {
    var f = this;
    return function() {
        var args = Array.prototype.slice.call(arguments);
        return f.apply(this, args.reverse());
    };
};

// =====================================================================================================================

/**
 * Predicate that returns true if an element is visible to the user.
 */
function isVisible(e) {
    return $(e).filter(':visible').css('visibility') != 'hidden'
}

(function ($) {
    // JQuery's filter() provides the index as the fn arg. This uses the element.
    $.fn.filterE = function (fn) {
        return this.filter(function(){return fn(this)})
    };

    $.fn.eachE = function (fn) {
        return this.each(function(){fn(this)})
    };

    $.fn.selfOrParent = function (css) {
        return ((this.filter(css).length != 0) ? this : (this.parents(css)));
    };
}(jQuery));

// =====================================================================================================================

var urls = new function() {
    this.viewUseCase = function(id){ return "/usecase/"+id }
    this.project = function(id){ return "/project/"+id }
};

// Add a global event handler to make Enter submit the current form, for any elements with class 'enterSubmitsForm'.
$(document).keypress(function (e) {
    if (e.which === 13 && e.target.classList.contains('enterSubmitsForm')) {
        e.preventDefault();
        e.stopPropagation();
        $(e.target).parents("form").find("input[type=submit]:visible:first").focus().click();
    }
})

DomEnhancements = [
    {css: "abbr.timeago", apply: function(x){ x.timeago() }},
    {css: "textarea",     apply: function(x){ x.autosize() }}
];

function registerDomEnhancementsWithLiveQuery() {
    for (var i = 0; i < DomEnhancements.length; i++) {
        var e = DomEnhancements[i]
        // console.debug("Registering LQ: "+ e.css)
        $(e.css).livequery(function (ee) {
            return function () {
                // console.debug("LQ calling: "+ ee.css)
                ee.apply($(this))
            }
        }(e))
    }
}
$(document).ready(registerDomEnhancementsWithLiveQuery)

function enhanceDom() { $(document).enhanceDom() }
(function ($) {
    // Provide JQuery fn to apply DomEnhancements
    $.fn.enhanceDom = function () {
        for (var i=0; i < DomEnhancements.length; i++) {
            var e = DomEnhancements[i]
            e.apply(this.find(e.css))
        }
        return this;
    };
}(jQuery));