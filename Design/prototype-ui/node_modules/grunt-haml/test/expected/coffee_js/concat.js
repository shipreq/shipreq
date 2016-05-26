(function() {
  if (window.HAML == null) {
    window.HAML = {};
  }

  window.HAML['coffee1'] = function(context) {
    return (function() {
      var $o;
      $o = [];
      $o.push("<!DOCTYPE html>\n<div id='main'>\n  <div class='note'>\n    <h2>Quick Notes</h2>\n    <ul>\n      <li>\n        Haml Coffee is usually indented with two spaces,\n        although more than two is allowed.\n        You have to be consistent, though.\n      </li>\n      <li>\n        The first character of any line is called\n        the \"control character\" - it says \"make a tag\"\n        or \"run CoffeeScript code\" or all sorts of things.\n      </li>\n      <li>\n        Haml takes care of nicely indenting your HTML.\n      </li>\n      <li>\n        Haml Coffee allows CoffeeScript code and blocks.\n      </li>\n    </ul>\n  </div>\n  <div class='note'>\n    You can get more information by reading the\n    <a href='https://github.com/netzpirat/haml-coffee'>\n      Official Haml Coffee Documentation\n    </a>\n  </div>\n  <div class='note'>\n    <p>\n      CoffeeScript code is included by using = at the\n      beginning of a line.\n    </p>\n    <p>\n      Read the tutorial for more information.\n    </p>\n  </div>\n</div>");
      return $o.join("\n").replace(/\s(?:id|class)=(['"])(\1)/mg, "");
    }).call(context);
  };

}).call(this);

(function() {
  if (window.HAML == null) {
    window.HAML = {};
  }

  window.HAML['coffee2'] = function(context) {
    return (function() {
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
      return $o.join("\n").replace(/\s([\w-]+)='true'/mg, ' $1').replace(/\s([\w-]+)='false'/mg, '').replace(/\s(?:id|class)=(['"])(\1)/mg, "");
    }).call(context);
  };

}).call(this);
