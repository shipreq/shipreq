function UseCaseSummary(uc) {
    var m = ko.mapping.fromJS(uc)

    m.cssClass = "uc-" + uc.eid

    m.editMode = ko.observable(false)

    m.enterEditMode = function(){ m.editMode(true); $("."+m.cssClass+" textarea").select().focus() }

    m.viewUrl = urls.viewUseCase(uc.eid)

    return m
}

function UCIViewModel(ucs) {
    this.useCases = ko.observableArray($.map(ucs,UseCaseSummary))
    this.populated = ko.computed(function(){ return this.useCases().length > 0 }, this);
    this.findByDataEid = function(v) { return $.grep(VM.useCases(), function(n){ return n.eid() == v })[0] }
}

$(document).ready(function() {
    ko.applyBindings(VM)
});

$(document).on('uc-add', function(event, data) {
    var m = UseCaseSummary(data)
    m.editMode(true)
    VM.useCases.push(m)
    m.enterEditMode()
});

$(document).on('uc-upd', function(event, data) {
    var n = UseCaseSummary(data)
    var m = VM.findByDataEid(n.eid())
    VM.useCases.replace(m,n)
});
