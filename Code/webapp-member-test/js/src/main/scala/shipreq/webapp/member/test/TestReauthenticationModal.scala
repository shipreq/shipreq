package shipreq.webapp.member.test

import japgolly.scalajs.react._
import japgolly.scalajs.react.test.Simulate
import org.scalajs.dom.{Element, document}
import shipreq.base.util._
import shipreq.webapp.base.data.Username
import shipreq.webapp.base.protocol.ajax.CommonProtocols.Login
import shipreq.webapp.base.protocol.webstorage.AbstractWebStorage
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.member.ui.ReauthenticationModal

class TestReauthenticationModal(initialResponse: Option[ErrorMsg \/ Permission]) {

  val localStorage = AbstractWebStorage.inMemory()

  var nextResponse: Option[ErrorMsg \/ Permission] =
    initialResponse

  var attempts: Vector[Login.Request] =
    Vector.empty

  val proc: ReauthenticationModal.AttemptLogin =
    p =>
      AsyncCallback.delay[AsyncCallback[ErrorMsg \/ Permission]] {
        attempts :+= p
        nextResponse match {
          case Some(response) => AsyncCallback.pure(response)
          case None           => AsyncCallback.never
        }
      }.flatten

  def modal(username: Username, rootDom: Element = document.body) =
    ReauthenticationModal(username, proc, rootDom, 0)(localStorage)
}

object TestReauthenticationModal {
  def apply(initialResponse: Option[ErrorMsg \/ Permission]): TestReauthenticationModal =
    new TestReauthenticationModal(initialResponse)

  final case class Obs(id: String)($$: DomZipperJs, t: TestReauthenticationModal) {
    private val $ = $$("#" + id)
    private val dom = $.domAsHtml

    val requestCount = t.attempts.length
    val isVisible    = dom.classList.contains("active")
    val password     = new CommonObs.Input($("input:password"))
    val loginButton  = $(".ui.button.primary").domAsButton
    val cancelButton = $(".ui.button:not(.primary)").domAsButton
  }

  final class TestDsl[R, O, S](final val * : Dsl[Id, R, O, S, String])(getObs: O => Obs) {
    private implicit def autoObs(o: O) = getObs(o)

    val requestCount = *.focus("Reauth request count").value(_.obs.requestCount)

    val isVisible = *.focus("ReauthenticationModal is visible").value(_.obs.isVisible)

    val password = *.focus("ReauthenticationModal password").value(_.obs.password.value)

    def setPassword(p: String): *.Actions =
      *.action(s"Set password to: $p")(_.obs.password.setValueDirect(p)) +> password.assert(p)

    val clickLogin: *.Actions =
      *.action("Click Login")(Simulate click _.obs.loginButton)

    val clickCancel: *.Actions =
      *.action("Click cancel")(Simulate click _.obs.cancelButton)
  }
}
