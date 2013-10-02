function titleSel() { return $('#project-title h1') }
function titleGet() { return titleSel().html() }
function titleSet(value) { titleSel().html(value) }

function renameSectionSel() { return $('#project-title .form') }
function renameInputSel() { return $('#project-title input.title') }
function renameInputSet(value) { renameInputSel().val(value) }

function renameCtlsVisibility(show) {
    titleSel().setVis(!show)
    renameSectionSel().setVis(show)
}

function renameStart() {
    renameCtlsVisibility(true)
    renameInputSel().focus().select()
    return false;
}

function renameCancel() {
    renameCtlsVisibility(false)
    renameInputSet(titleGet())
    return false;
}

$(document).on('project-updated', function(event, data) {
    titleSet(data)
    renameCancel()
});

$(document).ready(function() {
    $('#project-title button.cancel').click(renameCancel)
    $('nav .update').click(renameStart)
    $('nav .readucs').click(PENDING)
    $('nav .genpdf').click(PENDING)
    $('nav .delete').click(PENDING)
})