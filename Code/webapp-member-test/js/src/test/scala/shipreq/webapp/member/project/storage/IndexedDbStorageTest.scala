package shipreq.webapp.member.project.storage

import japgolly.scalajs.react.AsyncCallback
import shipreq.base.test.Node.asyncTest
import shipreq.webapp.member.project.data.{ClientSideProjectEncryptionKey, Project}
import shipreq.webapp.member.project.event.EventOrd
import shipreq.webapp.member.project.library.{Cache, CacheJs}
import shipreq.webapp.member.test.ProjectLibraryTestUtil._
import shipreq.webapp.member.test.WebappTestUtil.ImplicitProjectEqualityDeep._
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.member.test.project.UnsafeTypes.autoEventOrd
import shipreq.webapp.member.test.{TestEncryption, TestIndexedDb}
import sourcecode.Line
import utest._

object IndexedDbStorageTest extends TestSuite {
  import IndexedDbStorage._
  import TestData._

  private val nextIdbPrefix: AsyncCallback[String] = {
    var prev = 0
    AsyncCallback.delay {
      prev += 1
      "IndexedDbStorageTest-" + prev + "-"
    }
  }

  def newStorage(ctx: ClientSideStorage.Context, key: ClientSideProjectEncryptionKey, plCache: Cache = CacheJs()): AsyncCallback[IndexedDbStorage] =
    for {
      enc <- TestEncryption.engine(key.value)
      pre <- nextIdbPrefix
      s   <- IndexedDbStorage.nonStandard(TestIndexedDb.instance(), ctx, enc, pre, plCache)
    } yield s

  private def assertSavePlanResult(actual: (Map[EventOrd, Project], Set[EventOrd]))
                                  (expectToAdd: EventOrd*)
                                  (expectToDel: EventOrd*)
                                  (implicit l: Line): Unit = {
    type T = (Set[Int], Set[Int])
    val e: T = (expectToAdd.iterator.map(_.value).toSet, expectToDel.iterator.map(_.value).toSet)
    val a: T = (actual._1.keySet.map(_.value), actual._2.map(_.value))
    assertEq(a, e)
  }

  @elidable(elidable.FINEST)
  private def assertMilestoneCombination(inMem: Int, onDisk: Int, ok: Boolean)(implicit l: Line): Unit = {
    var actual = true
    try {
      IndexedDbStorage.assertMilestones(inMem = inMem, onDisk = onDisk)
    } catch {
      case _: Throwable => actual = false
    }
    assertEq(actual, ok)
  }

  override def tests = Tests {

    "savePlan" - {
      implicit def cache = newCache(5)
      val savePlan = new Internals(5).savePlan _

      "empty" - {
        val pl = newProjectLibrary(17, 20, 21)
        pl.projectAt(12) // forces milestones 5, 10
        val actual = savePlan(pl, ArraySeq.empty)
        assertSavePlanResult(actual)(5, 10, 17)()
      }

      "same" - {
        val pl = newProjectLibrary(20)
        pl.projectAt(19) // forces milestones 5, 10, 15
        val actual = savePlan(pl, ArraySeq(5, 10, 15, 20))
        assertSavePlanResult(actual)()()
      }

      "nextM" - {
        val pl = newProjectLibrary(11)
        pl.projectAt(10) // forces milestones 5, 10
        val actual = savePlan(pl, ArraySeq(5, 10))
        assertSavePlanResult(actual)(11)()
      }

      "next" - {
        val pl = newProjectLibrary(12)
        pl.projectAt(11) // forces milestones 5, 10
        val actual = savePlan(pl, ArraySeq(5, 10, 11))
        assertSavePlanResult(actual)(12)(11)
      }

      "preM" - {
        val pl = newProjectLibrary(10)
        pl.projectAt(5) // forces milestones 5
        val actual = savePlan(pl, ArraySeq(5, 9))
        assertSavePlanResult(actual)(10)(9)
      }

      "combo" - {
        val pl = newProjectLibrary(17, 20, 21)
        pl.projectAt(12) // forces milestones 5, 10
        val actual = savePlan(pl, ArraySeq(10, 11, 14))
        assertSavePlanResult(actual)(5, 17)(11, 14)
      }

      "older" - {
        val pl = newProjectLibrary(2)
        val actual = savePlan(pl, ArraySeq(3))
        assertSavePlanResult(actual)()()
      }

      "olderNoM" - {
        val pl = newProjectLibrary(12)
        val actual = savePlan(pl, ArraySeq(10, 13))
        assertSavePlanResult(actual)()()
      }

      "olderAddM" - {
        val pl = newProjectLibrary(12)
        pl.projectAt(10) // forces milestones 5, 10
        val actual = savePlan(pl, ArraySeq(10, 13))
        assertSavePlanResult(actual)(5)()
      }

      "olderM" - {
        val pl = newProjectLibrary(10)
        pl.projectAt(5) // forces milestones 5, 10
        val actual = savePlan(pl, ArraySeq(13))
        assertSavePlanResult(actual)(5, 10)()
      }
    }

    "loadMilestones" - asyncTest {
      val milestones = newMilestones()
      val cache      = newCache(MilestonesEvery, milestones)
      val pl1        = newProjectLibrary(MilestonesEvery + 4)
      val milestone  = pl1.projectAt(MilestonesEvery).get

      for {
        s <- newStorage(u1p1, key_u1p1, cache)
        _ <- s.saveProjectLibrary(pl1)
        _ <- AsyncCallback.delay(milestones.clear())
        _ <- s.getProjectLibrary
      } yield {
        assertEq(milestones.toList, milestone :: Nil)
      }
    }

    "assertMilestones" - {

      "same" -
        assertMilestoneCombination(onDisk = 100, inMem = 100, ok = true)

      "halfMem" -
        // load a bunch of stuff from disk only to discard immediately
        assertMilestoneCombination(onDisk = 200, inMem = 100, ok = false)

      "halfDisk" -
        // save every 2nd milestone - this is a fair possibility
        assertMilestoneCombination(onDisk = 100, inMem = 200, ok = true)
    }

  }
}
