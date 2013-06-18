function UseCaseSummary(uc) {
    var m = ko.mapping.fromJS(uc)

    m.cssClass = "uc-"+uc.id

    m.editMode = ko.observable(false)

    m.enterEditMode = function(){ m.editMode(true); $("."+m.cssClass+" textarea").select().focus() }

    m.save = submitJsonForm(apiUrls.updateUseCaseHeader(uc.id), function(result) {
        var n = UseCaseSummary(result)
        VM.useCases.replace(m,n)
        $(document).enhanceDom()
    })

    return m
}

function UCIViewModel(ucs) {
    this.useCases = ko.observableArray($.map(ucs,UseCaseSummary))
    this.populated = ko.computed(function(){ return this.useCases().length > 0 }, this);
}

$(document).ready(function() {
    ko.applyBindings(VM)
    $(document).enhanceDom()
});

$(document).on('new-uc', function(event, data) {
    var m = UseCaseSummary(data)
    m.editMode(true)
    VM.useCases.push(m)
    $(document).enhanceDom();
    m.enterEditMode()
});
