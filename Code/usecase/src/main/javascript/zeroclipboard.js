// require "vendor/ZeroClipboard.js"

ZeroClipboard.setDefaults( { moviePath: '/js/vendor/ZeroClipboard.swf' } );
clip = new ZeroClipboard( $("[data-clipboard-text], [data-clipboard-target]") );

clip.on( 'complete', function ( client, args ) {
    if (this != null && this !== undefined) {
        var t = $(this)
        var oldH = t.html()
        var oldT = t.attr('title')
        t.html('Copied')
        t.attr('title','')
        setTimeout(function(){
            t.html(oldH)
            t.attr('title',oldT)
        }, 2000)
    }
} );
