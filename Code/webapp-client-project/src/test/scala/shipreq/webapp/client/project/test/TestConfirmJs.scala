package shipreq.webapp.client.project.test

import japgolly.scalajs.react.CallbackTo
import shipreq.webapp.base.lib.ConfirmJs

final case class TestConfirmJs() extends ConfirmJs {
  var response = true

  private var _calls = 0
  def calls() = _calls

  override def apply(msg: String): CallbackTo[Boolean] =
    CallbackTo {
      _calls += 1
      response
    }
}
