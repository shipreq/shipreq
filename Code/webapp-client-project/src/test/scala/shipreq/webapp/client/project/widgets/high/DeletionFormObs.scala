package shipreq.webapp.client.project.widgets.high

import org.scalajs.dom.html
import shipreq.webapp.client.base.test.TestState._
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
    $("button", 1 of 2).domAs[html.Button]

  val cancelButton: html.Button =
    $("button", 2 of 2).domAs[html.Button]
}
