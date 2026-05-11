package shipreq.webapp.member.project.library

import japgolly.scalajs.react.CallbackTo
import java.time.Instant
import nyaya.gen.Gen
import scala.collection.immutable.TreeSet
import shipreq.webapp.base.util.Obfuscated
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.test.ProjectLibraryTestUtil._
import shipreq.webapp.member.test.WebappTestUtil.{newProject => _, _}
import shipreq.webapp.member.test.project.RandomEventStream
import utest._

object MutableProjectLibraryTest extends TestSuite {
  import ProjectLibrary.WithMetaData

  override def tests = Tests {
    import ImplicitProjectEqualityDeep._

    "props" - {
      val p1 = Project.init(Creator1)

      val genTest: Gen[(WithMetaData, Vector[VerifiedEvent], Project, WithMetaData)] = {
        val md1 = looseProjectMetaData(p1, eventsTotal = p1.ordAsInt, eventsInit = 0)
        val s1 = WithMetaData.init(Creator1, p1, md1, Obfuscated(""), Cache.Disabled(Creator1))
        for {
          (p2, ves) <- RandomEventStream.verifiedEvents(80).run(p1)
          batches   <- Gen.batches(ves, 0 to 7)
            .pair.map(x => x._1 ++ x._2) // duplicate all events (in different batches) to test idempotency
            .shuffle // shuffle to test commutivity
        } yield {
          val m = MutableProjectLibrary(s1)
          //        println(s"Generated ${ves.length} events and ${batches.length} batches starting at #${initialOrd.value + 1}")
          //        batches.foreach(println)
          //        m.addListener((ves, _, s) => Callback(println(s"Adding: $ves, pending: ${s.futureEventRange}")))
          batches.foreach(b => m.update(b.to(TreeSet)).runNow())
          (s1, ves, p2, m.get.runNow())
        }
      }

      val (s1, ves, p2, s2) = genTest.sample()

      assertEq("Total event count", s2.latestMetaData.eventsTotal, s1.latestMetaData.eventsTotal + ves.length)
      assertEq("Init event count", s2.latestMetaData.eventsInit, s1.latestMetaData.eventsInit)
      assertEq("Future events", s2.futureEvents.toList, Nil)
      assertEq("Latest EventOrd", s2.ord, Some(EventOrd.Latest(s2.latestMetaData.eventsTotal)))
      s2.latestMetaData.assertInSyncWith(p2)
      assertEq(s2.latest, p2)
    }

    "staleness" - {
      var t = Instant.now()
      val pl = MutableProjectLibrary.empty(Creator1, CallbackTo(t))
      def staleSince = pl.get.runNow().staleSince
      assertEq(staleSince, None)

      // 3 {}
      pl.update(newProject(3)).runNow()
      assertEq(staleSince, None)

      // 4 {}
      pl.update(newVerifiedEvents(4)).runNow()
      assertEq(staleSince, None)

      // 4 {6}
      pl.update(newVerifiedEvents(6)).runNow()
      assertEq(staleSince, Some(t))
      val t1 = t
      t = t.plusSeconds(1)

      // 4 {6, 9}
      pl.update(newVerifiedEvents(9)).runNow()
      assertEq(staleSince, Some(t1))

      // 6 {9}
      pl.update(newVerifiedEvents(5)).runNow()
      assertEq(staleSince, Some(t)) // staleSince changes because Project advanced
      t = t.plusSeconds(1)

      // 7 {9}
      pl.update(newVerifiedEvents(7)).runNow()
      assertEq(staleSince, Some(t)) // staleSince changes because Project advanced
      val t2 = t
      t = t.plusSeconds(1)

      // 7 {9, 10}
      pl.update(newVerifiedEvents(10)).runNow()
      assertEq(staleSince, Some(t2))

      // 10 {}
      pl.update(newVerifiedEvents(8)).runNow()
      assertEq(staleSince, None)
    }
  }
}
