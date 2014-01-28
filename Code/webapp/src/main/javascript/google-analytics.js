(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
})(window,document,'script','//www.google-analytics.com/analytics.js','ga');

var GA = (function(){

    var sendEvent = function(category, action, label, value) {
        // var s = '|'; console.log("GA EVENT:", category, s, action, s, label, s, value);
        ga('send', 'event', category, action, label, value);
    };

    var actionFn = function(category, action) {
        var f = function(){ sendEvent(category, action) };
        return function(){ return f };
    };

    var actionFnWithLabel = function(category, action) {
        return function(lbl){
            return function(){
                sendEvent(category, action, lbl)}}};

    var actions = {
        project: (function(){
            var name = 'Project';
            return {
                rename: actionFn(name, 'Rename'),
                delete: actionFn(name, 'Delete')
            };
        }()),
        UC: (function(){
            var name = 'Use Case';
            return {
                create: actionFn(name, 'Create'),
                update: actionFn(name, 'Update'),
                rename: actionFn(name, 'Rename'),
                readOwn: actionFn(name, 'Read own')
            };
        }()),
        UCE: (function(){
            var name = 'Use Case Editor';
            return {
                switchUC:   actionFn(name, 'Switch use case'),
                clickRef:   actionFn(name, 'Insert ref via label click'),
                help:       actionFnWithLabel(name, 'Help'),
                flowUpdate: actionFnWithLabel(name, 'Flow Update')
            };
        }()),
        NYI: (function(){
            var name = 'Unimplemented';
            return {
                delUC:   actionFn(name, 'Delete use case'),
                delUser: actionFn(name, 'Delete user'),
                genPdf:  actionFn(name, 'Generate PDF')
            };
        }())
    };

    var click = 'click';
    return {
        actions: actions,

        init: function() {
            // https://developers.google.com/analytics/devguides/collection/analyticsjs/
            var hostname = 'shipreq.com';
            if (window.location.hostname === hostname) {
                ga('create', 'UA-47038103-1', hostname);
            } else {
                ga('create', 'UA-47038103-2', {'cookieDomain': 'none'});
            }
            ga('set', 'page', GA.path());
        },

        path: function(path) {
            return (path || window.location.pathname)
                .replace(/(\/(?:register|resetpw|project|share|usecase)\/)[a-zA-Z0-9]+/,'$1___');
        },

        setupEventStats: function() {
            // URL: /
            $('#useracct-ctls button.delete').on(click, actions.NYI.delUser());

            // URL: /project/___
            var p = $('#project-main');
            if (p.length) {
                var i = p.find('header nav');
                i.find('a.readucs')    .on(click, actions.UC.readOwn());
                i.find('button.genpdf').on(click, actions.NYI.genPdf());
                i.find('button.update').on(click, actions.project.rename());
                i.find('li a.delete')  .on(click, actions.project.delete());
                p.find('#usecase-list')
                    .on(click, 'form button.update[type=submit]', actions.UC.rename())
                    .on(click, 'a.delete',                        actions.NYI.delUC())
                ;
                p.find('#usecase-new form button[type=submit]').on(click, actions.UC.create());
            }

            // URL: /usecase/___
            var uce = $('#uce');
            if (uce.length) {
                uce.on(click,'.steps .ctl button.add',       actions.UCE.flowUpdate('+'))
                   .on(click,'.steps .ctl button.delete',    actions.UCE.flowUpdate('-'))
                   .on(click,'.steps .ctl button.indentDec', actions.UCE.flowUpdate('«'))
                   .on(click,'.steps .ctl button.indentInc', actions.UCE.flowUpdate('»'))
                   .on(click,'.steps .addTailStep button',   actions.UCE.flowUpdate('±'))
                   .on(click,'#save',                        actions.UC.update())
                ;
                $('.navbar .ucs.dropdown li a').on(click, actions.UCE.switchUC());
                $('#help-links').on(click, function(e){ actions.UCE.help(e.target.innerText)() });
                // GA.actions.UCE.clickRef()() is performed in uce.js
            }
        }
    }
}());

GA.init();
ga('send', 'pageview');


/*
Event Tracking - Web Tracking
=============================

ga('send', 'event', 'category', 'action');
ga('send', 'event', 'category', 'action', 'label');
ga('send', 'event', 'category', 'action', 'label', value);  // value is a number.

The send command also accepts an optional field object as the last parameter for any of these commands. The field object
is a standard JavaScript object, but defines specific field names and values accepted by analytics.js.

For example, you might want to set the page field for a particular event. You do this using:

    ga('send', 'event', 'category', 'action', {'page': '/my-new-page'});

Similarly, you might want to send an event, but not impact your bounce rate. This is easily solved by configuring the
event to be a non-interaction event using the following code:

    ga('send', 'event', 'category', 'action', {'nonInteraction': 1});

The Events Tracking Variables
=============================
Each Event you track comes with five different variables you set to help you track and group different sets of events,
which are described below.  Since I’ve started talking about downloading PDFs, let’s use an example where you have a
Support Library on your site with various PDF files (Getting Started Guide, Technical Reference, Trouble Shooting Guide,
etc.).  You want to track how often each is downloaded.

Category (required) – Think of this as an overall grouping of similar events.  So in our example, it could be “Support
    Library”.  Each of the different documents in your Support Library would have this as their category.  But if you
    had other documents available for download in different sections of your site, a Price List for example, those might
    have Category values more relevant to the type of document they are (perhaps “Sales Collateral” for the Price List
    example).

Action (required) – This gets more to what is actually being done.  In our example, the user is downloading a PDF, so it
    could be “Download”.  If there are other sections of downloads on your site, you could easily monitor the total
    number of downloads across all categories this way.

Label (optional) – We get a little more specific here.  If the Category is an overall group of things, Label is the
    individual item in that group.  So in this example, one could be “Getting Started Guide” and another could be
    “Technical Reference”.

Value (optional) – It’s helpful that Google allows us to set a value to an action.  In some cases you may not choose to
    set a value, but if for example you have a Price List available for download somewhere, that might be of greater
    value than a Support Document because the downloading of a Price List might indicate a qualified prospect.  The
    Value parameter must be an integer.  It could represent a financial amount, but does not have to.  Let’s say we’ve
    decided that a download of the Getting Started Guide is worth a value of 50, which if nothing else would be relative
    to other values we set for other events.

*/