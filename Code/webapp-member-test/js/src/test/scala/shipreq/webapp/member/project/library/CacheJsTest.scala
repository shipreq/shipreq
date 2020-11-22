package shipreq.webapp.member.project.library

import java.time.Instant
import scala.scalajs.js
import shipreq.webapp.base.util.LruMemo
import shipreq.webapp.member.project.data.Project
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.test.WebappTestUtil.ImplicitProjectEqualityDeepExceptEventTime._
import shipreq.webapp.member.test.WebappTestUtil._
import sourcecode.Line
import utest._

object CacheJsTest extends TestSuite {

  override def tests = Tests {

    val now = Instant.now()

    def event(i: Int) =
      VerifiedEvent(
        EventOrd(i),
        Event.ProjectNameSet(i.toString),
        now.plusSeconds(i))

    def events(is: Range) =
      VerifiedEvent.NonEmptySeq.force(
        VerifiedEvent.Seq.empty ++ is.iterator.map(event))

    def projectAt(n: Int) =
      Project.empty.updateOrThrow(events(1 to n))

    val milestones: js.Array[Project] =
      new js.Array

    val milestones2: CacheJs.ArrayExt[Project] =
      milestones.asInstanceOf[CacheJs.ArrayExt[Project]]

    val retainEvery = 3

    var cache = new CacheJs.NonEmpty(
      latest         = projectAt(9),
      milestoneEvery = retainEvery,
      milestones     = milestones,
      lru            = LruMemo.ExternalFn.byUnivEq[Int, Project](1),
    )

    def lru = cache.lru

    def inMilestoneCache(): Set[Int] =
      milestones
        .indices
        .iterator
        .filter(milestones2.get(_).isDefined)
        .map(_ * retainEvery + retainEvery)
        .toSet

    def inLruCache(): Set[Int] = {
      val b = Set.newBuilder[Int]
      lru.foreachKey(b += _)
      b.result()
    }

    def assertCaches(inMilestones: Int*)(inLru: Int*)(implicit l: Line): Unit = {
      val actual = (inMilestoneCache(), inLruCache())
      val expect = (inMilestones.toSet, inLru.toSet)
      assertEq(actual, expect)
    }

    def get(ord: Int)(implicit l: Line): Project = {
      val actual = cache(EventOrd(ord)).getOrThrow("No result for ord " + ord)
      val expect = projectAt(ord)
      assertEq(actual, expect)
      actual
    }

    assertCaches()()
    get(5)
    assertCaches(3)(5)
    get(5)
    assertCaches(3)(5)
    get(4)
    assertCaches(3)(4)

    cache = cache.update(projectAt(15))
    get(4)
    assertCaches(3)(4)
    get(11)
    assertCaches(3, 6, 9)(11)
    get(15)
    assertCaches(3, 6, 9, 12, 15)(11)
    get(15)
    assertCaches(3, 6, 9, 12, 15)(11)
    get(8)
    assertCaches(3, 6, 9, 12, 15)(8)
    get(9)
    assertCaches(3, 6, 9, 12, 15)(8)

    milestones.clear()
    get(13)
    assertCaches(9, 12)(13)
    get(6)
    assertCaches(3, 6, 9, 12)(13)
    get(4)
    assertCaches(3, 6, 9, 12)(4)
  }
}
