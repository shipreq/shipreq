
window.HAML['fixtures/js/js1'] = function(locals){
function html_escape(text) {
    return (text + "").
      replace(/&/g, "&amp;").
      replace(/</g, "&lt;").
      replace(/>/g, "&gt;").
      replace(/\"/g, "&quot;");
  }
with(locals || {}) {
  try {
   var _$output="<?xml version='1.0' encoding='utf-8' ?>\n<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n<html xmlns=\"http://www.w3.org/1999/xhtml\"><head>\n<script type=\"text/javascript\">\n//<![CDATA[\nfunction greet(message) {\n  alert(\"Message from MCP: \" + message);\n}\n//]]>\n</script>\n<title>Script and Css test</title><style type=\"text/css\">\nbody {\n  color: pink;\n}\n</style></head><body onload=\"greet(&quot;I'm Pink&quot;)\">COLOR ME PINK</body></html>";
 return _$output;  } catch (e) {
    return "\n<pre class='error'>" + html_escape(e.stack) + "</pre>\n";
  }
}
}
