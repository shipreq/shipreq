/**
 * Moves a step (and its future children) from Normal Course to Alternate Courses.
 * 
 * @param stepId The ID of the step to move.
 * @param rewriteFn The function that updates steps' labels and indents.
 */
function nc_to_ac(stepId, rewriteFn) {
	var t = $('<div class="stepTransport"></div>')
				.uniqueId()
				.appendTo('#courses-n td')
				.append($('#' + stepId + ', #' + stepId + '~.step'))
	t.hide("drop", {direction : "down"}, 250, function() {
		rewriteFn();
		t.prependTo($('#courses-a td'))
			.show("drop", {direction : "up"}, 250, function() {
				t.children().prependTo($('#courses-a td'));
				t.remove();
			});
	});
}