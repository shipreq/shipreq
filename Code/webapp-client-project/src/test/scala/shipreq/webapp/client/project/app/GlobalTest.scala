package shipreq.webapp.client.project.app

import java.time.Duration
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.client.project.test.{TestGlobal, TestWebWorkerClient}
import shipreq.webapp.client.ww.api._
import shipreq.webapp.member.project.event.Event.ProjectNameSet
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.project.protocol.websocket.ProjectSpaProtocols.WsReqRes
import shipreq.webapp.member.test.ProjectLibraryTestUtil._
import utest._

object GlobalTest extends TestSuite {

  private implicit def univEqWwCmd: UnivEq[WebWorkerCmd[_]] =
    UnivEq.force

  class SyncTest {
    val t = TestGlobal()

    val initialNextOrd = t.nextEventOrd.runNow()

    def newEvent(i: Int): VerifiedEvent = {
      val e = ProjectNameSet("event " + i)
      val v = t.verifyEventsCB(e).runNow().head
      val o = initialNextOrd + i
      v.copy(ord = o)
    }

    def addEvents(i: Int*): Unit =
      t.addEvents(VerifiedEvent.Seq.empty ++ i.map(newEvent)).runNow()

    def syncIfStaleForMs(ms: Long) =
      t.requestSyncIfStaleFor(Duration.ofMillis(ms)).runNow()

    def assertSyncRequest(ordOffsets: Int*): Unit = {
      val req = t.reqs().last.assertType(WsReqRes.Sync)
      val expect = NonEmptySet.force(ordOffsets.iterator.map(initialNextOrd + _).toSet)
      assertEq(req, expect)
    }
  }

  override def tests = Tests {
    "sync" - {
      val st = new SyncTest; import st._

      "simple" - {
        addEvents(1)
        t.advanceTimeByMs(100)
        syncIfStaleForMs(20)
        t.assertReqsSent(1)
        assertSyncRequest(0)
      }

      "two" - {
        addEvents(1)
        addEvents(2)
        t.advanceTimeByMs(100)
        addEvents(4)
        syncIfStaleForMs(150)
        t.assertReqsSent(0)

        t.advanceTimeByMs(100)
        syncIfStaleForMs(150)
        t.assertReqsSent(1)
        assertSyncRequest(0, 3)
      }

      "clear" - {
        addEvents(1)
        t.advanceTimeByMs(100)
        addEvents(0)
        t.advanceTimeByMs(100)
        syncIfStaleForMs(20)
        t.assertReqsSent(0)
      }

      "overlap" - {
        //    |0 1 2 3 4 5
        // t0 |  *     *
        // t1 -*--|  *
        // t2     (S)
        // t3 -----*----|

        // t0
        addEvents(1, 4)

        // t1
        t.advanceTimeByMs(100)
        addEvents(0, 3)

        // t2
        t.advanceTimeByMs(100)
        syncIfStaleForMs(150)
        t.assertReqsSent(0) // because we did advance Project in the last 150ms - we only want to sync on failure to advance

        syncIfStaleForMs(50)
        t.assertReqsSent(1)
        assertSyncRequest(2)

        // t3
        t.advanceTimeByMs(100)
        addEvents(2)
        syncIfStaleForMs(1)
        t.assertReqsSent(1) // i.e. no new reqs sent
      }
    }

    "wwMissingEvents" - {
      val ww = TestWebWorkerClient()
      val g = TestGlobal(newProject(4), ww)
      g.addEvents(newVerifiedEvents(6, 7)).runNow()
      assertEq(ww.requestCount(), 0)

      ww.push(WebWorkerPushCmd.MissingEvents(NonEmptySet(3, 4, 7, 8).map(EventOrd.apply)))
      assertEq(ww.requestCount(), 1)
      assertEq(ww.lastRequest(), Some(WebWorkerCmd.UpdateProject(\/-(newVerifiedEvents(3, 4, 7)))))
    }
  }
}
