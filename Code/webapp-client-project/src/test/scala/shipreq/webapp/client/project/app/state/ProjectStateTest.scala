package shipreq.webapp.client.project.app.state

import japgolly.microlibs.stdlib_ext.StdlibExt._
import nyaya.gen.Gen
import scala.annotation.tailrec
import utest._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event._
import shipreq.webapp.base.test.WebappTestUtil._

object ProjectStateTest extends TestSuite {

  // TODO Move into Nyaya
  def Gen_batches[A](as: Vector[A], partitionSize: Range.Inclusive, keepRemainder: Boolean = true): Gen[Vector[Vector[A]]] = {
    val genSize = Gen.chooseInt(partitionSize.min, partitionSize.max)
    Gen { ctx =>
      val b = Vector.newBuilder[Vector[A]]
      @tailrec def go(rem: Vector[A]): Unit =
        if (rem.isEmpty)
          ()
        else if (rem.length >= partitionSize.min) {
          val n = genSize.run(ctx)
          b += rem.take(n)
          go(rem.drop(n))
        } else if (keepRemainder)
          b += rem
      go(as)
      b.result()
    }
  }

  override def tests = Tests {

    val p1 = Project.empty

    val genTest: Gen[(ProjectState, Vector[VerifiedEvent], Project, ProjectState)] =
      for {
        initEventCount    ← Gen.chooseInt(4)
        totalEvents1      ← Gen.chooseInt(40).map(_ + initEventCount)
        latestOrd1        = Option.when(totalEvents1 > 0)(EventOrd.Latest(totalEvents1))
        pao1              = ProjectAndOrd(p1, latestOrd1)
        s1                = ProjectState.init(pao1, looseProjectMetaData(p1, totalEventCount = totalEvents1, initEventCount = initEventCount))
        ((p2, _), ves)    ← RandomEventStream.verifiedEvents(140).run((p1, pao1.nextOrd))
        batches           ← Gen_batches(ves, 0 to 7)
                              .pair.map(x => x._1 ++ x._2) // duplicate all events (in different batches) to test idempotency
                              .shuffle // shuffle to test commutivity
      } yield {
        val m = new ProjectState.Mutable(s1)
//        println(s"Generated ${ves.length} events and ${batches.length} batches starting at #${initialOrd.value + 1}")
//        batches.foreach(println)
//        m.addListener((ves, _, s) => Callback(println(s"Adding: $ves, pending: ${s.futureEventRange}")))
        batches.foreach(b => m.applyEventSeqCB(b.to).runNow())
        (s1, ves, p2, m.state())
      }

    val (s1, ves, p2, s2) = genTest.sample()

    assertEq("Total event count", s2.projectMetaData.totalEventCount, s1.projectMetaData.totalEventCount + ves.length)
    assertEq("Init event count", s2.projectMetaData.initEventCount, s1.projectMetaData.initEventCount)
    assertEq("Future events", s2.futureEvents.keySet.toList, Nil)
    assertEq("Latest EventOrd", s2.ord, Some(EventOrd.Latest(s2.projectMetaData.totalEventCount)))
    s2.projectMetaData.assertInSyncWith(p2)
    assertEq(s2.project, p2)
  }
}
