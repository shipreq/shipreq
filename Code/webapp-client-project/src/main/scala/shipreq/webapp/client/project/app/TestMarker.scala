package shipreq.webapp.client.project.app

import japgolly.scalajs.react.vdom.html_<^._

/**
  * Additional attributes are sometimes added to DOM so that unit tests can find it (the DOM) and test it without
  * depending on an unstable CSS path.
  */
object TestMarker {

  private var next = 0

  private def create(): TestMarker = {
    val id = ('a' + next).toChar.toString
    next += 1
    new TestMarker(id)
  }

  val deletionForm = create()
  val restorationForm = create()
  val useCaseStepLabel = create()
  val useCaseStepText = create()
  val useCaseTailStep = create()
}

final class TestMarker private[TestMarker] (id: String) {
  val name = "data-tm-" + id
  val tagMod = VdomAttr(name) := 1
}