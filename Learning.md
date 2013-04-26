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

