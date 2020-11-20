package shipreq.webapp.member.project.library

import nyaya.gen.Gen
import scala.collection.immutable.TreeSet
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.event._
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.member.test.project.RandomEventStream
import utest._

object MutableProjectLibraryTest extends TestSuite {

  override def tests = Tests {
    import ImplicitProjectEqualityDeep._

    val p1 = Project.empty

    val genTest: Gen[(ProjectLibrary, Vector[VerifiedEvent], Project, ProjectLibrary)] = {
      val s1 = ProjectLibrary.init(p1, looseProjectMetaData(p1, eventsTotal = p1.history.ordAsInt, eventsInit = 0))
      for {
        (p2, ves) <- RandomEventStream.verifiedEvents(80).run(p1)
        batches   <- Gen.batches(ves, 0 to 7)
                      .pair.map(x => x._1 ++ x._2) // duplicate all events (in different batches) to test idempotency
                      .shuffle // shuffle to test commutivity
      } yield {
        val m = new MutableProjectLibrary(s1)
//        println(s"Generated ${ves.length} events and ${batches.length} batches starting at #${initialOrd.value + 1}")
//        batches.foreach(println)
//        m.addListener((ves, _, s) => Callback(println(s"Adding: $ves, pending: ${s.futureEventRange}")))
        batches.foreach(b => m.applyEventSeqCB(b.to(TreeSet)).runNow())
        (s1, ves, p2, m.state())
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
}
