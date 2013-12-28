// =====================================================================================================================
// Functional JS
// http://dailyjs.com/2012/09/14/functional-programming/

/*
  Partial Application
  ===================
  var plus_two = function(x,y) { return x+y; };
  var add_three = plus_two.p(3);
  add_three(4); // 7
 */
Function.prototype.p = function() {
    var args = Array.prototype.slice.call(arguments);
    var f = this;
    return function() {
        var inner_args = Array.prototype.slice.call(arguments);
        return f.apply(this, args.concat(inner_args))
    };
};

/*
  Composition
  ===========
  var greet = function(s) { return 'hi, ' + s; };
  var exclaim = function(s) { return s + '!'; };
  var excited_greeting = greet.c(exclaim);
  excited_greeting('Pickman') // hi, Pickman!
 */
Function.prototype.c = function(g) {
    var f = this;
    return function() {
        var args = Array.prototype.slice.call(arguments);
        return f.call(this, g.apply(this, args));
    };
};

/*
  Flipping
  ========
  var div = function(x,y) { return x / y; };
  div(1, 2) // 0.5
  div.f()(1,2) // 2
 */
Function.prototype.f = function() {
    var f = this;
    return function() {
        var args = Array.prototype.slice.call(arguments);
        return f.apply(this, args.reverse());
    };
};

/*
Curries a function so that the first arg is the "this" that one would get if it were done by hand.
 */
Function.prototype.withThis = function() {
    var f = this;
    return function() {
        var args = Array.prototype.slice.call(arguments);
        return f.apply(this, [this].concat(args));
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

    $.fn.show2 = function () {
        return $(this).removeClass('hidden2');
    };

    $.fn.hide2 = function () {
        return $(this).addClass('hidden2');
    };

    $.fn.setVis = function (show) {
        if (show === undefined) show = true
        return show ? $(this).show2() : $(this).hide2()
    };

    $.fn.scrollTo = function (duration) {
        //if (show === undefined) show = true
        var t = $(this)
        $('html,body').animate({ scrollTop: t.first().offset().top }, duration)
        return t
    };
    $.fn.addClassOnHover = function (tgt, className) {
        tgt = $(tgt)
        $(this).hover(
            function(){tgt.addClass(className)},
            function(){tgt.removeClass(className)}
        )
    };
    $.fn.attrAdd = function (name, newValue) {
        var t = $(this);
        var e = t.attr(name);
        e = (e === undefined) ? '' : (e+' ');
        t.attr(name, e + newValue);
        return t;
    };
}(jQuery));

// =====================================================================================================================

function setupViz(callback) {
    if (typeof(VizWorker) == 'undefined') {
        VizWorker = new Worker('/js/viz-worker.js')
        VizWorker.onmessage = function(ev) {
            var d = ev.data
            $(d.tgt).html(d.svg)

            if (callback !== undefined)
                callback(d)
        }
    }
}

// =====================================================================================================================

$(document).on('dynmodal', function (event, data) {
    var d = $(data)
        .appendTo("body")
        .on('hidden.bs.modal', function () {
            $(this).remove();
        })
        .on('shown.bs.modal', function () {
            $(this).find('.focus:first').focus();
        });

    // Disable button(s) until user types in confirmation text
    d.find('.type-to-confirm').eachE(function (r){
        r = $(r)
        var inp = r.find('[data-confirmation]')
        if (inp.length == 1) {
            var btn = r.find('.confirmed')
            var norm = function (s) { return $.trim(s).replace(/\s+/g, " ")  }
            var exp = norm(inp.data('confirmation'))
            var f = function () { btn.prop('disabled', norm(inp.val()) != exp) }
            f();
            inp.on('input propertychange change', f)
        } else
            console.warn("Length != 1: ",inp)
    });

    d.modal('show');
});

// =====================================================================================================================

function PENDING() { alert('PENDING'); return false }

// Make sure this is in sync with AppConfig.scala
AppConfig = {
    AppName: "ShipReq"
}

// Make sure this is in sync with AppSiteMap#mkTitle
function mkTitle(title) { return title + " | " + AppConfig.AppName}

// Add a global event handler to make Enter submit the current form, for any elements with class 'enterSubmitsForm'.
$(document).keypress(function (e) {
    if (e.which === 13 && e.target.classList.contains('enterSubmitsForm')) {
        e.preventDefault();
        e.stopPropagation();
        var b = $(e.target).parents("form").find("[type=submit]:visible")
        //console.log(b)
        b.first().focus().click();
    }
})

DomEnhancements = [
    {css: "abbr.timeago",      apply: function(x){ x.timeago() }},
    {css: "abbr.timeago2",     apply: function(x){ x.timeago2() }},
    {css: "time.showdate",     apply: function(x){ x.showdate() }},
    {css: "time.showdatetime", apply: function(x){ x.showdatetime() }},
    {css: "textarea",          apply: function(x){ x.autosize() }}
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

function enhanceDom() { $(document).enhanceDom() }

(function ($) {

    // Customised impl of timeago.
    // Attr should have ISO8601 in the title and nothing in the text.
    // After using this, a "3. days ago"-like expression will be visible with a locale-friendly string shown
    // when hovered over.
    $.fn.timeago2 = function () {
        var e = $(this)
        var isotime = e.attr('title')
        if (typeof isotime === 'string') {
            var d = new Date(isotime)
            //var t = d.toLocaleString()
            var t = d.toLocaleDateString() + ' @ ' + d.toLocaleTimeString()
            e.html(t).timeago()
        }
        else console.warn("timeago2 failed on ", e)
    }

    $.fn.showdate = function () {
        return applyShowDateGen("showdate", $(this), function(d){
            return d.toLocaleDateString()})
    }

    $.fn.showdatetime = function () {
        return applyShowDateGen("showdatetime", $(this), function(d){
            return d.toLocaleDateString() + ' ' + d.toLocaleTimeString()})
    }

    // Provide JQuery fn to apply DomEnhancements
    $.fn.enhanceDom = function () {
        for (var i=0; i < DomEnhancements.length; i++) {
            var e = DomEnhancements[i]
            e.apply(this.find(e.css))
        }
        return this;
    };

}(jQuery));

// =====================================================================================================================
// TODO Clean this shit file up!

function applyShowDateGen(name, e, showFn) {
    var isotime = e.attr('datetime')
    if (typeof isotime === 'string') {
        var d = new Date(isotime)
        var show = showFn(d)
        var ago = $.timeago(d)
        e.attr('title', ago)
        e.html(show)
    }
    else console.warn(name + " failed on ", e)
}

/** @const */ var ucFilterForm = {
    setup: function() {
        // UC Filter form: Only show sub-content for selected option.
        $('.ucfilter-group input.ucfilter').change(ucFilterForm.updateAll)
    },
    updateAll: function() {
        $('.ucfilter-group div.ucfilter').eachE(ucFilterForm.update)
    },
    update: function(e) {
        var sub = $(e).find('.sub')
        if (sub) {
            var open = sub.is(':visible')
            var selected = $(e).find('input.ucfilter').is(':checked')
            if (open != selected) {
                sub.toggle("slide",{direction:'up'}, 200)
            }
        }
    }
}

// =====================================================================================================================
// Published UCs (share-view & read-own-ucs)

/** @const */ var publishedUcs = {
    renderAll: function() {
        var data_dot_attr = 'dot'
        $('tr.flowgraph').each(function(i,p){
            var id = "fg-"+i
            $(p).attrAdd('id', id)
            var dot = $(p).find('[data-'+data_dot_attr+']').data(data_dot_attr)
            var tgtSel = '#'+id+' td'
            VizWorker.postMessage({tgt:tgtSel, dot:dot})
        })
    },
    setup: function() {
        if ($('.ucs-published').length != 0) {
            setupViz()
            publishedUcs.renderAll()
        } else if ($('.preloadviz').length != 0)
            setupViz();
    }
}

// =====================================================================================================================

$(document).ready(function(){

    registerDomEnhancementsWithLiveQuery();

    ucFilterForm.setup();
    publishedUcs.setup();

    $('#share-list .urltxt').click(function(){ $(this).select() });

    // When refs are hovered over, highlight the reference step.
    $('.ucs-published .steps tr')
        .filter(function(i,e){return e.id.substr(0,4)=="step"})
        .each(function(i,e){ $('.ucs-published a.step[href=#'+e.id+']').addClassOnHover('#'+e.id,'highlight') })
})
