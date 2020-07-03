package shipreq.webapp.client.project.test

import japgolly.scalajs.react.CallbackTo
import shipreq.webapp.base.lib.ConfirmJs

final case class TestConfirmJs() extends ConfirmJs {
  var nextResponse = true

  private var _calls = 0
  def calls() = _calls

  override def apply(msg: String): CallbackTo[Boolean] =
    CallbackTo {
      _calls += 1
      nextResponse
    }
}

object TestConfirmJs {
  import shipreq.webapp.base.test.TestState._

  final class Obs(t: TestConfirmJs) {
    val calls = t.calls()
  }

  final class TestDsl[R, O, S](val * : Dsl[Id, R, O, S, String])
                              (getRef: R => TestConfirmJs,
                               getObs: O => Obs) {
    private implicit def autoRef(r: R): TestConfirmJs = getRef(r)
    private implicit def autoObs(o: O): Obs = getObs(o)

    val calls = *.focus("ConfirmJs calls").value(_.obs.calls)

    def setNextResponse(r: Boolean): *.Actions =
      *.action("Set next ConfirmJs response to " + r)(_.ref.nextResponse = r)
  }
}
