package shipreq.webapp.client.lib.ui

import japgolly.scalajs.react.ScalazReact._
import scalaz.effect.IO
import utest._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.client.lib.{FailureIO, SuccessIO}
import shipreq.webapp.client.test.TestUtil._

object DeletionTest extends TestSuite {

  case class MockState(status: RowStatus)
  val setRowStatus: Persistence.SetRowStatus[MockState] = r => ReactS.modT(_.copy(status = r))

  override def tests = TestSuite {
    var cb: Option[(SuccessIO, FailureIO)] = None
    var s = MockState(RowStatus.Sync)
    val r = (st: ReactST[IO, MockState, Unit]) => ReactS.unlift(st).exec(s).map { s2 => s = s2}
    val d = Persistence.asyncDelete[MockState]((s, f) => IO{cb = (s,f).some}, r, setRowStatus)
    val dio = r(d)

    'uninvoked {
      assert(cb.isEmpty)
      assertEq(s.status, RowStatus.Sync)
    }

    'invoked {
      dio.unsafePerformIO()
      assert(cb.isDefined)
      assertEq(s.status, RowStatus.Locked)
    }

    'rpcSuccess {
      dio.unsafePerformIO()
      cb.get._1.io.unsafePerformIO()
      assertEq(s.status, RowStatus.Locked) // success = nop
    }

    'rpcFailure {
      dio.unsafePerformIO()
      cb.get._2.io.unsafePerformIO()
      val retry = assertRowStatusFailed(s.status).retry
      retry.unsafePerformIO()
      assertEq(s.status, RowStatus.Locked)
    }
  }
}
