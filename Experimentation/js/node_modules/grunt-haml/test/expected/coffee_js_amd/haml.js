(function() {
  define(['jquery', 'underscore', 'templates/path/to'], function($, _, to) {
    return function(context) {
      var render;
      render = function() {
        var $c, $e, $o;
        $e = function(text, escape) {
          return ("" + text).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/'/g, '&#39;').replace(/\//g, '&#47;').replace(/"/g, '&quot;');
        };
        $c = function(text) {
          switch (text) {
            case null:
            case void 0:
              return '';
            case true:
            case false:
              return '' + text;
            default:
              return text;
          }
        };
        $o = [];
        $o.push("<article>\n  <h1>" + ($e($c("hello " + this.greet))) + "</h1>\n  <section id='note'>I've an id</section>\n  <div class='" + (['am', 'i', "" + ($e($c(this["class"])))].sort().join(' ').replace(/^\s+|\s+$/g, '')) + "'>I've three* classes</div>\n</article>");
        $o.push("" + $c(require("templates/path/to")()));
        return $o.join("\n").replace(/\s([\w-]+)='true'/mg, ' $1').replace(/\s([\w-]+)='false'/mg, '').replace(/\s(?:id|class)=(['"])(\1)/mg, "");
      };
      return render.call(context);
    };
  });

}).call(this);
