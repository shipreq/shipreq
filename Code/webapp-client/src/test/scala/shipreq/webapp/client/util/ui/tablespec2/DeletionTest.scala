package shipreq.webapp.client.util.ui.tablespec2

import japgolly.scalajs.react.ScalazReact._
import shipreq.webapp.client.protocol.FailureIO
import shipreq.webapp.client.util.ui.table.SuccessIO
import shipreq.base.util.ScalaExt._
import TestUtil._
import utest._
import NeoSaves._

import scalaz.effect.IO

object DeletionTest extends TestSuite {

  case class MockState(status: RowStatus)
  val setRowStatus: SetRowStatus[MockState] = r => ReactS.modT(_.copy(status = r))

  override def tests = TestSuite {
    var cb: Option[(SuccessIO, FailureIO)] = None
    var s = MockState(RowStatus.Sync)
    val r = (st: ReactST[IO, MockState, Unit]) => ReactS.unlift(st).exec(s).map { s2 => s = s2}
    val d = deleteAsync[MockState]((s, f) => IO{cb = (s,f).some}, r, setRowStatus)
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
