// JQuery is required separately.
// - 1) It's 93KB, separation allows for me parallelism.
// - 2) Will probably switch to CDN later.

// require "vendor/jquery-ui.js"
// require "vendor/jquery-autosize.js"
// require "vendor/jquery-timeago.js"
// require "vendor/jquery-serializeObject.js"

var apiUrls = new function() {
    this.updateUseCaseHeader = function(id){ return "/api/usecase/"+id }
}

function ajaxErrorHandler(xhr, textStatus, errorThrown) {
    var msg = "Something went wrong. Please try again."
        + "\nIf the problem persists, reload the page and give it another try."
        + "\n\nError " + xhr.status + ": " + errorThrown + "."
    if ([409,412,422,423,428].indexOf(xhr.status) >= 0 && xhr.responseText != "") {
        var limit = 50
        var r = xhr.responseText
        if (r.length > limit) r = r.substring(0, limit) + "..."
        msg += "\nFeedback: " + r
    }
    alert(msg)
}

function submitJsonForm(url, type, successCallback) {
    return function(form) {
        $.ajax({
            url: url,
            type: type,
            contentType: 'application/json',
            dataType: 'json',
            data: JSON.stringify($(form).serializeObject()),
            error: ajaxErrorHandler,
            success: function(data, textStatus, xhr) {
                var result = JSON.parse(xhr.responseText)
                successCallback(result)
            }
        })
    }
}

function enterSubmitsFormHandler(e) {
    if (e.which === 13) {
        e.preventDefault();
        e.stopPropagation();
        $(e.target).parents("form").find("input[type=submit]").focus().click();
    }
}

(function ($) {
    $.fn.enhanceDom = function () {
        this.find("abbr.timeago").timeago();
        this.find('textarea').autosize();
        this.find('.enterSubmitsForm').keypress(enterSubmitsFormHandler);
        return this;
    };
}(jQuery));

$(document).ready(function () {
    $(document).enhanceDom();
});
