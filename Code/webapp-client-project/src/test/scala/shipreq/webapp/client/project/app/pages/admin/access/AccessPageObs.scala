package shipreq.webapp.client.project.app.pages.admin.access

import org.scalajs.dom.html
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.project.test._

final class AccessPageObs($: DomZipperJs, val global: TestGlobal.Obs, val confirmJs: TestConfirmJs.Obs) {

  private lazy val leaveProjectSegment = $.child(".segment", 1 of 1)
  lazy val leaveProjectButton          = leaveProjectSegment("button").domAs[html.Button]
  lazy val leaveProjectButtonLoading   = leaveProjectButton.classList.contains("loading")
}
