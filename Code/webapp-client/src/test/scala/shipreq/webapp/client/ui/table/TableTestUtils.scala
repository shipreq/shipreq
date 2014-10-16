package shipreq.webapp.client.ui.table

import japgolly.scalajs.react.ComponentStateFocus
import utest._

object TableTestUtils {

  sealed trait RowStatusT
  case object Sync extends RowStatusT
  case object Locked extends RowStatusT
  case object Failed extends RowStatusT
  object RowStatusT {
    def apply(r: RowStatus) = r match {
      case RowStatus.Sync      => Sync
      case RowStatus.Locked    => Locked
      case RowStatus.Failed(_) => Failed
    }
  }

  case class TableAssertions[S, D, P](spec: TableSpec[_, S, D, _, P, _, _], c: ComponentStateFocus[S]) {

    val initialState = c.state

    def resetState(): Unit = c setState initialState

    def test[A](f: => A): A = {
      resetState()
      f
    }

    def assertRowValues[A](f: (RowStatus, P) => A) = new {
      def apply(m: (D, A)*): Unit = {
        val actual = spec.savedGet(c).map(r => r.d -> f(r.status, r.p)).toMap
        val expect = m.toMap
        assert(actual == expect)
      }
    }

    def assertRowStatuses =
      assertRowValues((r, p) => RowStatusT(r))

    def assertUnsavedRowStatus(expect: Option[RowStatusT]) = {
      val actual = spec.unsavedGet(c).map(u => RowStatusT(u.status))
      assert(actual == expect)
    }
  }
}
