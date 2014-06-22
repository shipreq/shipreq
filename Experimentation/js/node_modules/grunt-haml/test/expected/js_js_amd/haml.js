define(['jquery','underscore','templates/path/to'], function($,_,to) { 
return function(locals){
function html_escape(text) {
    return (text + "").
      replace(/&/g, "&amp;").
      replace(/</g, "&lt;").
      replace(/>/g, "&gt;").
      replace(/\"/g, "&quot;");
  }
with(locals || {}) {
  try {
   var _$output=" <span> This will have space in and around it. </span>  <span> This will, too. </span>  <span> "+
"also works with code".toUpperCase()+
" </span> <div class=\"conditional\">" + 
"";var a = "strings are truthy"; _$output = _$output  +
"";if(a){; _$output = _$output  +
"<div class=\"hello\"></div>" +
"";} else{; _$output = _$output  +
"<div class=\"goodbye\"></div>" +
"";}; _$output = _$output  + 
"</div>" +
"";var area = 0.5 * length * height; _$output = _$output  +
"<div class=\"area\">" + 
area + 
"</div>" +
require("templates/path/to")();
 return _$output;  } catch (e) {
    return "\n<pre class='error'>" + html_escape(e.stack) + "</pre>\n";
  }
}
};
});
