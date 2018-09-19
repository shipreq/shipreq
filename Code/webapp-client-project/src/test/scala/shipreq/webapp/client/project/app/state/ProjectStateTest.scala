package shipreq.webapp.client.project.app.state

import japgolly.microlibs.nonempty.NonEmptyVector
import japgolly.scalajs.react.Callback
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
        initialOrd        ← Gen.chooseInt(100).map(EventOrd(_))
        initialEventCount ← Gen.chooseInt(100)
        initialState      = ProjectState.init(p1, looseProjectMetaData(p1, eventCount = initialEventCount), initialOrd)
        ((p2, _), ves)    ← RandomEventStream.verifiedEvents(140).run((p1, initialOrd + 1))
        batches           ← Gen_batches(ves, 0 to 7)
                              .pair.map(x => x._1 ++ x._2) // duplicate all events (in different batches) to test idempotency
                              .shuffle // shuffle to test commutivity
      } yield {
        val m = new ProjectState.Mutable(initialState)
//        println(s"Generated ${ves.length} events and ${batches.length} batches starting at #${initialOrd.value + 1}")
//        batches.foreach(println)
//        m.addListener((ves, _, s) => Callback(println(s"Adding: $ves, pending: ${s.futureEventRange}")))
        batches.foreach(b => m.applyEventSeqCB(b.to).runNow())
        (initialState, ves, p2, m.state())
      }

    val (s1, ves, p2, s2) = genTest.sample()

    assertEq("Event count", s2.projectMetaData.eventCount, s1.projectMetaData.eventCount + ves.length)
    assertEq("Latest EventOrd", s2.latestEventOrd, s1.latestEventOrd + ves.length)
    assertEq("Future events", s2.futureEvents.keySet.toList, Nil)
    s2.projectMetaData.assertInSyncWith(p2)
    assertEq(s2.project, p2)
  }
}
