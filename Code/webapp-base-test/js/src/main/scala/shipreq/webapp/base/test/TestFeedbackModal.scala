package shipreq.webapp.base.test

import japgolly.scalajs.react._
import org.scalajs.dom.{Element, document}
import scalaz.\/
import shipreq.base.util._
import shipreq.webapp.base.protocol.CommonProtocols.SubmitFeedback
import shipreq.webapp.base.ui.FeedbackModal

class TestFeedbackModal(initialResponse: Option[ErrorMsg \/ Unit]) {

  var nextResponse: Option[ErrorMsg \/ Unit] =
    initialResponse

  var attempts: Vector[SubmitFeedback.UserInput] =
    Vector.empty

  val proc: FeedbackModal.SubmitFn =
    p =>
      AsyncCallback.point[AsyncCallback[ErrorMsg \/ Unit]] {
        attempts :+= p
        nextResponse match {
          case Some(response) => AsyncCallback.pure(response)
          case None           => AsyncCallback.never
        }
      }.flatten

  def modal(rootDom: Element = document.body) =
    FeedbackModal(proc, rootDom)
}

object TestFeedbackModal {
  def apply(initialResponse: Option[ErrorMsg \/ Unit]): TestFeedbackModal =
    new TestFeedbackModal(initialResponse)
}