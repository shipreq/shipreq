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
				.appendTo(courses+'-n td')
				.append($('#' + stepId + ', #' + stepId + '~.step'))
	t.hide("drop", {direction : "down"}, 250, function() {
		rewriteFn();
		t.prependTo($(courses+'-a td'))
			.show("drop", {direction : "up"}, 250, function() {
				t.children().prependTo($(courses+'-a td'));
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
				.prependTo(courses+'-a td')
				.append($(stepsJqExpr))
	t.hide("drop", {direction : "up"}, 250, function() {
		rewriteFn();
		t.appendTo($(courses+'-n td'))
			.show("drop", {direction : "down"}, 250, function() {
				t.children().appendTo($(courses+'-n td'));
				t.remove();
			});
	});
}
