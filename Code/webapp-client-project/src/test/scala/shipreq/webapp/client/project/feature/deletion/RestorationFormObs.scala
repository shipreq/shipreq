package shipreq.webapp.client.project.feature.deletion

import org.scalajs.dom.html
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.app.TestMarker

object RestorationFormObs {

  def option($: DomZipper): Option[RestorationFormObs] =
    $.findSelfOrChildWithAttribute(TestMarker.restorationForm.name)
      .map(new RestorationFormObs(_))
}

class RestorationFormObs($: DomZipper) {

  val restoreButton: html.Button =
    $("button:contains(Restore)").domAs[html.Button]

  val cancelButton: html.Button =
    $("button:contains(Cancel)").domAs[html.Button]
}
