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

/**
 * If valid field has focus, then applies a fn to it.
 */
function withFocusedInputField(fn) {
    var allSelected = getAllUceInputFields().filter(':focus')
    if (allSelected.length > 0) fn(allSelected[0])
}

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

function onAltDown() { return changeFocus(getElementBelow) }
function onAltUp()   { return changeFocus(getElementAbove) }
function onEscape()  { return withFocusedInputField(blurFn) }
$(document).ready(function() {
    Mousetrap.bindGlobal('alt+down', onAltDown);
    Mousetrap.bindGlobal('alt+up',   onAltUp);
    Mousetrap.bindGlobal('esc',      onEscape);
});
