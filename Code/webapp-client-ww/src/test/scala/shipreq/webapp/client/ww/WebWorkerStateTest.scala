package shipreq.webapp.client.ww

import japgolly.microlibs.testutil.TestUtil._
import japgolly.scalajs.react.AsyncCallback
import java.time.Instant
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.{Event, EventOrd, ProjectAndOrd, VerifiedEvent}
import shipreq.webapp.base.test.UnsafeTypes.autoSomeEventOrdLatest
import sourcecode.Line
import utest._

object WebWorkerStateTest extends TestSuite {

  private class Promise {
    var called = 0

    def assertPending()(implicit l: Line): Unit =
      assertEq(called, 0)

    def assertComplete()(implicit l: Line): Unit =
      assertEq(called, 1)
  }

  private val now =
    Instant.now()

  override def tests = Tests {
    val s = new WebWorkerState

    def setProject(ord: Int): Unit =
      s.setProject(ProjectAndOrd(Project.empty, ord)).runNow()

    def updateProject(ords: Int*): Unit = {
      val ves = VerifiedEvent.Seq.empty ++ ords.map(i => VerifiedEvent(EventOrd(i), Event.ProjectNameSet(i.toString), now))
      s.updateProject(VerifiedEvent.NonEmptySeq.force(ves)).runNow()
    }

    def await(ord: Option[EventOrd.Latest]): Promise = {
      val p = new Promise
      // Have to run it because the creation has been flattened with the promise itself
      (s.await(ord) >> AsyncCallback.delay(p.called += 1)).toCallback.runNow()
      p
    }

    def pendingPromiseCount(): Int =
      s.getState.runNow().ordPromises.size

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

    }
  }
}
