package shipreq.webapp.client.project.widgets

import org.scalajs.dom.html
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.TestMarker

object DeletionFormObs {

  def option($: DomZipper): Option[DeletionFormObs] =
    $.findSelfOrChildWithAttribute(TestMarker.deletionForm.name)
      .map(new DeletionFormObs(_))
}

class DeletionFormObs($: DomZipper) {

  val reasonEditor: html.TextArea =
    $("textarea").domAs[html.TextArea]

  val deleteButton: html.Button =
    $("button:contains(Delete)").domAs[html.Button]

  val cancelButton: html.Button =
    $("button:contains(Cancel)").domAs[html.Button]
}
