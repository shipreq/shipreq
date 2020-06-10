package shipreq.webapp.base.test

import japgolly.scalajs.react._
import org.scalajs.dom.{Element, document}
import scalaz.\/
import shipreq.base.util._
import shipreq.webapp.base.protocol.ajax.CommonProtocols.Login
import shipreq.webapp.base.ui.ReauthenticationModal
import shipreq.webapp.base.user.Username

class TestReauthenticationModal(initialResponse: Option[ErrorMsg \/ Permission]) {

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
    ReauthenticationModal(username, proc, rootDom, 0)
}

object TestReauthenticationModal {
  def apply(initialResponse: Option[ErrorMsg \/ Permission]): TestReauthenticationModal =
    new TestReauthenticationModal(initialResponse)
}