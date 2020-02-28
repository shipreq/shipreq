package shipreq.webapp.client.project.app.pages.config_old.shared

import japgolly.scalajs.react.{Callback, CallbackTo}
import japgolly.scalajs.react.ScalazReact._
import utest._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data.TCB
import shipreq.webapp.client.project.test.TestUtil._

object DeletionTest extends TestSuite {

  case class MockState(status: RowStatus)
  val setRowStatus: Persistence.SetRowStatus[MockState] = r => ReactS.modT(_.copy(status = r))

  override def tests = Tests {
    var cb: Option[(TCB.Success, TCB.Failure)] = None
    var s = MockState(RowStatus.Sync)
    val r = (st: ReactST[CallbackTo, MockState, Unit]) => ReactS.unlift(st).exec(s).map { s2 => s = s2}
    val d = Persistence.asyncDelete[MockState]((s, f) => Callback{cb = (s,f).some}, r, setRowStatus)
    val dio = r(d)

    'uninvoked {
      assert(cb.isEmpty)
      assertEq(s.status, RowStatus.Sync)
    }

    'invoked {
      dio.runNow()
      assert(cb.isDefined)
      assertEq(s.status, RowStatus.Locked)
    }

    'rpcSuccess {
      dio.runNow()
      cb.get._1.runNow()
      assertEq(s.status, RowStatus.Locked) // success = nop
    }

    'rpcFailure {
      dio.runNow()
      cb.get._2.runNow()
      val retry = assertRowStatusFailed(s.status).retry
      retry.runNow()
      assertEq(s.status, RowStatus.Locked)
    }
  }
}
