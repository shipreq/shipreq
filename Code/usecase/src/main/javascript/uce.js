/**
 * Moves a step (and its future children) from Normal Course to Alternate Courses.
 *
 * @param stepId The ID of the step to move.
 * @param rewriteFn The function that updates steps' labels and indents.
 */
function nc_to_ac(containerId, stepId, rewriteFn) {
	var courses = containerId + ' .courses'
	var t = $('<div class="stepTransport"></div>')
				.uniqueId()
				.appendTo(courses+'-n td.steps')
				.append($('#' + stepId + ', #' + stepId + '~.step'))
	t.hide("drop", {direction : "down"}, 250, function() {
		rewriteFn();
		t.prependTo($(courses+'-a td.steps'))
			.show("drop", {direction : "up"}, 250, function() {
				t.children().prependTo($(courses+'-a td.steps'));
				t.remove();
			});
	});
}

/**
 * Moves steps from Alternate Courses to Normal Course.
 *
 * @param stepsJqExpr A JQuery expression that selects all steps to move.
 * @param rewriteFn The function that updates steps' labels and indents.
 */
function ac_to_nc(containerId, stepsJqExpr, rewriteFn) {
	var courses = containerId + ' .courses'
	var t = $('<div class="stepTransport"></div>')
				.uniqueId()
				.prependTo(courses+'-a td.steps')
				.append($(stepsJqExpr))
	t.hide("drop", {direction : "up"}, 250, function() {
		rewriteFn();
		t.appendTo($(courses+'-n td.steps'))
			.show("drop", {direction : "down"}, 250, function() {
				t.children().appendTo($(courses+'-n td.steps'));
				t.remove();
			});
	});
}

// ---------------------------------------------------------------------------------------------------------------------

function getAllUceInputFields() { return $('.uce textarea:visible') }

function getElementBeside(elementId, offset) {
    var all = getAllUceInputFields()
    for (var i = 0; i < all.length; i++) {
        if (elementId == all[i].id) {
            i += offset + all.length
            i %= all.length
            return all[i]
        }
    }
}

/**
 * Locates the textarea above the one given. If the top is given, then the bottom is returned.
 *
 * @param elementId The current element.
 * @return HTMLTextAreaElement
 */
function getElementAbove(elementId) { return getElementBeside(elementId, -1) }

/**
 * Locates the textarea below the one given. If the bottom is given, then the top is returned.
 *
 * @param elementId The current element.
 * @return HTMLTextAreaElement
 */
function getElementBelow(elementId) { return getElementBeside(elementId, 1) }

function getFocusedInputField() { return getAllUceInputFields().filter(':focus')[0] }

/** If an input field has focus, then applies a fn to it. */
function withFocusedInputField(fn) {
    var s = getFocusedInputField()
    if (s) fn(s)
}

/** Locates the top-level container of an element in a step. */
function getStepContainer(innerElement) {
    return $(innerElement).parents('.step')
}

/** Searches the given step-container's tree of elements for the add-step button. */
function getAddStepButton(stepContainer) { return stepContainer.find('button.add:visible')[0] }

/**
 * Changes the keyboard focus to another.
 *
 * @param tgtFn Given the ID of the currently focused field, returns the target field to move to focus to.
 *              (id: String) => (jQuery expression / HTMLTextAreaElement)
 */
function changeFocus(tgtFn) {
    withFocusedInputField(function (sel) {
        var tgt = tgtFn(sel.id)
        $(tgt).focus()
    })
}

function blurFn(sel) { return $(sel).blur() }

// ---------------------------------------------------------------------------------------------------------------------

/**
 * Returns the full label (eg. "1.0.2.a") of a step.
 *
 * @param element Any element within a step.
 * @returns A string.
 */
function getFullLabel(element) {
    var curStep = getStepContainer(element)
    var result = curStep.find('.lbl span').text()
    var lvl = curStep.data('lvl')
    if (lvl > 0) {
        // Get all steps before current
        var steps = curStep.parent('.steps').find('.step:not(#' + curStep.attr('id') + ' ~ *)')
        while(lvl-- != 0) {
            var lbl = steps.filter('[data-lvl='+lvl+']').last().find('.lbl span').text()
            result = lbl + "." + result
        }
    }
    return result
}

function makeRef(labelText) { return "[" + labelText + "]" }

DomEnhancements.push({css: "#uce textarea", apply: configureUceTextarea})
function configureUceTextarea(e) {
    e.on('focus', autoSetTypingMode)
    e.on('blur', autoSetTypingMode)
}

function inTypingMode() { return $('#uce').hasClass('typing') }

function setTypingMode(on) {
    var e = $('#uce')
    var c = 'typing'
    if (on) e.addClass(c); else e.removeClass(c)
}

function autoSetTypingMode() { setTypingMode(getFocusedInputField()) }

function onLabelClick(event) {
    if (inTypingMode()) {
        var label = getFullLabel(event.toElement)
        if (label && label.length > 0) {
            withFocusedInputField(function(focused){
                focused = $(focused)
                var ref = makeRef(label)
                var fullText = focused.val()
                var sel = focused.getSelection()
                var before = fullText.substr(0, sel.start)
                var after = fullText.substring(sel.end, fullText.length)
                if (before.match(/\S$/)) ref = " " +ref
                if (after.match(/^\S/)) ref += " "
                focused.replaceSelectedText(ref)
            })
        }
        event.preventDefault();
        event.stopPropagation();
    }
}

// ---------------------------------------------------------------------------------------------------------------------

function onAltDown() { return changeFocus(getElementBelow) }
function onAltUp()   { return changeFocus(getElementAbove) }
function onEscape()  { return withFocusedInputField(blurFn) }

function onAltEnter() {
    withFocusedInputField(function (textarea) {
        var addButton = getAddStepButton(getStepContainer(textarea))
        if (addButton) {
            $(textarea).blur()
            $(addButton).click()
        }
    })
}

function uceSetup() {
    Mousetrap.bindGlobal('alt+down',  onAltDown);
    Mousetrap.bindGlobal('alt+up',    onAltUp);
    Mousetrap.bindGlobal('alt+enter', onAltEnter);
    Mousetrap.bindGlobal('esc',       onEscape);
    $('.step .lbl, .step .lbl *').bindOnce('mousedown',onLabelClick)
}
$(document).ready(uceSetup)