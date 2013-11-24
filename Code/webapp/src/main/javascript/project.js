// ---------------------------------------------------------------------------------------------------------------------
// Project Title

function navbarProjectLinkSel() { return $('.navbar a.project') }
function navbarProjectLinkSet(value) { navbarProjectLinkSel().html(value) }

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

// ---------------------------------------------------------------------------------------------------------------------
// UC List

function ucListSection() { return $('#usecase-list') }
function ucListSel() { return ucListSection().children('ul') }
function ucLiSel(className) { return ucListSel().find('.'+className) }

function ucListSectionRefreshMode() {
    var empty = ucListSel().children().length == 0
    ucListSection().find('.none').setVis(empty)
    ucListSel().setVis(!empty)
}

function ucLiModeSet(el, viewMode) {
    var p = $(el).parents('.uc')
    p.find('.view-mode').setVis(viewMode)
    p.find('.edit-mode').setVis(!viewMode)
    return p
}
function ucLiModeView(el) {
    ucLiModeSet(el, true)
    return false;
}

function ucLiModeEdit(el) {
    ucLiModeSet(el, false).find('.edit-mode :text').focus().select()
    return false;
}

function ucliPrepare(scope) {
    scope.find('.view-mode button.update').click(ucLiModeEdit.withThis())
    scope.find('.edit-mode button.cancel').click(ucLiModeView.withThis())
    scope.find('a.delete').click(PENDING)
}

// =====================================================================================================================
// Triggers

$(document).on('project-updated', function(event, data) {
    titleSet(data)
    navbarProjectLinkSet(data)
    renameCancel()
});

$(document).on('usecase-created', function(event, data) {
    var d = $(data)
    d.appendTo(ucListSel()).scrollTo(300).effect('highlight',1000)
    $('#usecase-new :text').val("")
    ucliPrepare(d)
    ucListSectionRefreshMode()
});

$(document).on('usecase-updated', function(event, data) {
    ucLiSel(data.eid).replaceWith(eval(data.li))
    ucliPrepare(ucLiSel(data.eid).effect('highlight',400))
});

$(document).on('usecase-update-nop', function(event, data) {
    ucLiModeView(ucLiSel(data))
});

// =====================================================================================================================

$(document).ready(function() {
    $('#project-title button.cancel').click(renameCancel)
    $('nav .update').click(renameStart)
    $('nav .genpdf').click(PENDING)
    ucliPrepare(ucListSel())
    ucListSectionRefreshMode()
})
