Processes
=========
* Difficulut algorithms/complex code design.
  * Model data by hand in Excel.
  * Apply use cases.
  * Write algorithm in psuedo-code.
  * Write skeleton in code. Start with interfaces, and high-level impl of logic.
    This will usually turn up problems with data flow (ie. method params, class
    fields).
  * Write code and tests. Start with low-level, work to high.

* App
  * Requirements & scope.
  * Write use cases for entire app.
  * Prototype app with pen & paper (or similar).
  * Turn prototype into real screens with real dimensions and nothing missing.
    (Inkscape for mobile, real HTML for web).
  * Test use cases with prototype.
  * Move to impl.

Lift
====
* <body data-lift-content-id="main">
  This means crop file contents to #main.
  Without a surround attribute in #main there is no <html>, just the #main elem

* Snippet variable scope
  Object - Global.
  Class  - Per-request (including subsequent Ajax requests).
  object x extends RequestVar[Int](0) - Per-request.
  object x extends SessionVar[Int](0) - Per-session.

* Bindings in net.liftweb.util
    ValueCell, DynamicCell, FuncCell
    val total = price.lift(_ * qty)
* Application:
    "#total" #> WiringUI.asText(total)


* Ajax
  * Singleton snippets with closures.
  * StatefulSnippets.
  * ajaxForm(bind( id->SHtml.tag

  * Make form data-lift="form.ajax"
  * Add <button type="submit">Submit</button>
  * Add hidden(onSubmitCallback)
