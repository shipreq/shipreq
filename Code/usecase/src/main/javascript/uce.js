/**
 * Moves a step (and its future children) from Normal Course to Alternate Courses.
 *
 * @param stepId The ID of the step to move.
 * @param rewriteFn The function that updates steps' labels and indents.
 */
function nc_to_ac(containerId, stepId, rewriteFn, completionFn) {
	var courses = containerId + ' .courses'
	var t = $('<div class="stepTransport"></div>')
				.uniqueId()
				.appendTo(courses+'-n td.steps')
				.append($('#' + stepId + ', #' + stepId + '~.step'))
	t.hide("drop", {direction : "down"}, 250, function() {
		rewriteFn()
		t.prependTo($(courses+'-a td.steps'))
			.show("drop", {direction : "up"}, 250, function() {
				t.children().prependTo($(courses+'-a td.steps'))
				t.remove()
				completionFn()
			});
	});
}

/**
 * Moves steps from Alternate Courses to Normal Course.
 *
 * @param stepsJqExpr A JQuery expression that selects all steps to move.
 * @param rewriteFn The function that updates steps' labels and indents.
 */
function ac_to_nc(containerId, stepsJqExpr, rewriteFn, completionFn) {
	var courses = containerId + ' .courses'
	var t = $('<div class="stepTransport"></div>')
				.uniqueId()
				.prependTo(courses+'-a td.steps')
				.append($(stepsJqExpr))
	t.hide("drop", {direction : "up"}, 250, function() {
		rewriteFn()
		t.appendTo($(courses+'-n td.steps'))
			.show("drop", {direction : "down"}, 250, function() {
				t.children().appendTo($(courses+'-n td.steps'))
				t.remove()
				completionFn()
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

/**
 * If an input field has focus, then applies a fn to it.
 * @return False if not field found, else the result of the fn.
 */
function withFocusedInputField(fn) {
    var s = getFocusedInputField()
    return (s ? fn(s) : false)
}

/** Locates the top-level container of an element in a step. */
function getStepContainer(innerElement) {
    return $(innerElement).parents('.step')
}

/** Filter predicate that returns true for visible elements. */
function visible(i,e) {return $(e).filter(':visible').css('visibility') != 'hidden'}

function findSingleVisibleChild(parent, childCss) { return parent.find(childCss).filter(visible)[0] }

/** Searches the given step-container's tree of elements for the add-step button. */
function getAddStepButton(stepContainer) { return findSingleVisibleChild(stepContainer, 'button.add') }

/** Searches the given step-container's tree of elements for the increment-indent button. */
function getIncIndentButton(stepContainer) { return findSingleVisibleChild(stepContainer, 'button.indentInc') }

/** Searches the given step-container's tree of elements for the decrement-indent button. */
function getDecIndentButton(stepContainer) { return findSingleVisibleChild(stepContainer, 'button.indentDec') }

/**
 * Changes the keyboard focus to another.
 *
 * @param tgtFn Given the ID of the currently focused field, returns the target field to move to focus to.
 *              (id: String) => (jQuery expression / HTMLTextAreaElement)
 * @return Whether the focus was changed.
 */
function changeFocus(tgtFn) {
    withFocusedInputField(function (sel) {
        var tgt = tgtFn(sel.id)
        $(tgt).focus()
        return true;
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
                focused.replaceSelectedText(ref).trigger('autosize.resize')
            })
        }
        event.preventDefault();
        event.stopPropagation();
    }
}

/**
 * If a step is focused, clicks one of its buttons.
 *
 * @param containerToButtonFn A fn that takes a step container and returns a button to click.
 * @param clickFn If provided, a function that takes a button and simulates clicking.
 * @return Whether a button was clicked.
 */
function clickFocusedStepsButton(containerToButtonFn, clickFn) {
    withFocusedInputField(function (textarea) {
        var button = containerToButtonFn(getStepContainer(textarea))
        if (button) {
            $(textarea).blur()
            if (clickFn)
                clickFn(button)
            else
                $(button).click()
            return true
        } else
            return false
    })
}

/** Invokes the onClick JS code in a button's attribute and adds additional args to the request. */
function clickLiftAjaxButtonWithExtraArgs(button, args) {
    var js = $(button).attr('onclick').replace('lift_ajaxHandler("','lift_ajaxHandler("'+args+'&').replace('return ','')
    eval(js)
}

/**
 * Invokes the onClick JS code in a button's attribute, adding a 'focus' flag to the request.
 * This flag directs the server function to focus a step as part of the response.
 */
 function clickWithFocusFlag(button){ clickLiftAjaxButtonWithExtraArgs(button, 'focus=true') }

// ---------------------------------------------------------------------------------------------------------------------

function onEscape()   { withFocusedInputField(blurFn) }
function onAltDown()  { changeFocus(getElementBelow) }
function onAltUp()    { changeFocus(getElementAbove) }
function onAltLeft()  { clickFocusedStepsButton(getDecIndentButton, clickWithFocusFlag) }
function onAltRight() { clickFocusedStepsButton(getIncIndentButton, clickWithFocusFlag) }
function onAltEnter() { clickFocusedStepsButton(getAddStepButton) }

function bindKeys(keys, fn) { Mousetrap.bindGlobal(keys, function(){ fn(); return false }) }

function uceSetup() {
    bindKeys('alt+right', onAltRight);
    bindKeys('alt+left',  onAltLeft);
    bindKeys('alt+down',  onAltDown);
    bindKeys('alt+up',    onAltUp);
    bindKeys('alt+enter', onAltEnter);
    bindKeys('esc',       onEscape);
}
$(document).ready(uceSetup)

$(document).on('focus', "#uce textarea", autoSetTypingMode)
$(document).on('blur',  "#uce textarea", autoSetTypingMode)
$(document).on('mousedown', ".step .lbl, .step .lbl *", onLabelClick)

//window.onbeforeunload = function() {
//    return "ANY UNSAVED CHANGES WILL BE LOST."
//}
