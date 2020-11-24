package shipreq.webapp.client.ww

import japgolly.scalajs.react.{AsyncCallback, Callback, CallbackTo}
import shipreq.webapp.base.config.AssetManifest
import shipreq.webapp.base.lib.LoggerJs
import shipreq.webapp.client.ww.api.WebWorkerCmd
import shipreq.webapp.member.project.event.EventOrd
import shipreq.webapp.member.test.ProjectLibraryTestUtil._
import shipreq.webapp.member.test.TestClientSideStorage
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

  private final class TestLogic extends WorkerState.Logic {
    private var _graphVizLoads = 0
    private var _importedScripts = Vector.empty[String]

    val css               = TestClientSideStorage()
    def graphVizLoads()   = _graphVizLoads
    def importedScripts() = _importedScripts

    override val importScriptList = ss => Callback { _importedScripts ++= ss }
    override val loadGraphViz     = _ => CallbackTo { _graphVizLoads += 1; null }
    override val cssProvider      = TestClientSideStorage.provide(css)
  }

  private val am = AssetManifest(None)

  // ===================================================================================================================

  override def tests = Tests {
    val testLogic = new TestLogic
    val s = new WorkerState(testLogic, LoggerJs.off)
    import testLogic._

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

    // -----------------------------------------------------------------------------------------------------------------

    "init" - {
      import TestClientSideStorage._

      val cmd = WebWorkerCmd.Init(am, u1p1, key_u1p1)

      def init(): Unit = {
        s.init(cmd).runNow()
        assertEq(graphVizLoads(), 1)
        assertEq(importedScripts(), Vector1(am.wwJs))
      }

      "fresh" - {
        init()
        assertEq(s.ordAsInt(), 0)
      }

      "cached" - {
        css.saveProjectLibrary(newProjectLibrary(3)).runNow()
        init()
        assertEq(s.ordAsInt(), 3)
      }

      "idempotent" - {
        css.saveProjectLibrary(newProjectLibrary(1)).runNow()
        init()
        assertEq(s.ordAsInt(), 1)

        css.saveProjectLibrary(newProjectLibrary(2)).runNow()
        init()
        assertEq(s.ordAsInt(), 1)
      }
    }

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
