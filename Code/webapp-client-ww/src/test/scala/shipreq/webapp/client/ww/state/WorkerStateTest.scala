package shipreq.webapp.client.ww.state

import japgolly.scalajs.react.AsyncCallback
import shipreq.webapp.member.project.event.EventOrd
import shipreq.webapp.member.test.ProjectLibraryTestUtil._
import shipreq.webapp.member.test.WebappTestUtil.{newProject => _, _}
import shipreq.webapp.member.test.project.UnsafeTypes.autoSomeEventOrdLatest
import sourcecode.Line
import utest._

object WorkerStateTest extends TestSuite {

  private class Promise {
    var called = 0

    def assertPending()(implicit l: Line): Unit =
      assertEq(called, 0)

    def assertComplete()(implicit l: Line): Unit =
      assertEq(called, 1)
  }

  override def tests = Tests {
    val s = new WorkerState()

    def setProject(ord: Int): Unit =
      s.update(-\/(newProject(ord))).runNow()

    def updateProject(ords: Int*): Unit = {
      val ves = newVerifiedEvents(ords: _*)
      s.update(\/-(ves)).runNow()
    }

    def await(ord: Option[EventOrd.Latest]): Promise = {
      val p = new Promise
      // Have to run it because the creation has been flattened with the promise itself
      (s.await(ord) >> AsyncCallback.delay(p.called += 1)).toCallback.runNow()
      p
    }

    def pendingPromiseCount(): Int =
      s.pendingPromiseCount()

    "await" - {

      "before" - {
        setProject(2)
        val p1 = await(5)
        val p2 = await(6)
        assertEq(pendingPromiseCount(), 2)
        p1.assertPending()
        p2.assertPending()

        updateProject(3, 4)
        p1.assertPending()
        p2.assertPending()

        updateProject(5)
        p1.assertComplete()
        p2.assertPending()
        assertEq(pendingPromiseCount(), 1)

        updateProject(6, 7)
        p1.assertComplete()
        p2.assertComplete()
        assertEq(pendingPromiseCount(), 0)

        updateProject(8)
        p1.assertComplete()
        p2.assertComplete()
      }

      "equal" - {
        setProject(2)
        val p = await(2)
        assertEq(pendingPromiseCount(), 0)
        p.assertComplete()

        updateProject(3)
        p.assertComplete()
      }

      "after" - {
        setProject(20)
        val p = await(19)
        assertEq(pendingPromiseCount(), 0)
        p.assertComplete()

        updateProject(21)
        p.assertComplete()
      }

      "empty" - {
        val p1 = await(0)
        val p2 = await(1)
        assertEq(pendingPromiseCount(), 1)
        p1.assertComplete()
        p2.assertPending()

        updateProject(1)
        assertEq(pendingPromiseCount(), 0)
        p1.assertComplete()
        p2.assertComplete()
      }

      "delayedInit" - {
        val p1 = await(11)
        val p2 = await(12)
        val p3 = await(13)
        assertEq(pendingPromiseCount(), 3)

        setProject(12)
        p1.assertComplete()
        p2.assertComplete()
        p3.assertPending()

        updateProject(13)
        p1.assertComplete()
        p2.assertComplete()
        p3.assertComplete()
      }

    }
  }
}
