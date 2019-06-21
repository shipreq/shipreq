package shipreq.webapp.base.issue

import nyaya.gen.Gen
import scala.annotation.tailrec
import utest._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.{EventSeqSummary, RandomEventStream, VerifiedEvent}
import shipreq.webapp.base.test.WebappTestUtil._

object IncrementalDetectionTest extends TestSuite {

  private val genEvents = RandomEventStream.justEntireEventStream(128)

  private def assertIncrementsEqualWhole(windowSize: Int, ves: Vector[VerifiedEvent], seed: Long): Unit = {
    @tailrec
    def go(it: IssueTracker, remainingEvents: Vector[VerifiedEvent], taken: Int): Unit =
      if (remainingEvents.nonEmpty) {
        val es     = remainingEvents.take(windowSize)
        val p2     = applyVerifiedEventSuccessfully(it.project, es: _*)
        val it2    = it.update(es.iterator.map(_.event), p2)
        val taken2 = taken + windowSize
        val expect = IssueTracker(p2)

        onFail {
          assertIssueSet(s"[windowSize=$windowSize, seed = ${seed}L , events=$taken]",
            actual = it2.issues.vector.map(_.issue),
            expect = expect.issues.vector.map(_.issue))
        } {
          println(EventSeqSummary(es.iterator.map(_.event)))
//          println("Actual:")
//          it2.issues.vector.map(_.issue).map("  - " + _).foreach(println)
//          println("Expect:")
//          expect.issues.vector.map(_.issue).map("  - " + _).foreach(println)
//          println(p2.config.tags.prettyPrint)
          for (i <- 0 until taken2) {
            val e = ves(i)
            printf("%2d: %s\n", i, e.event.toString.take(160).replace('\n', ' '))
          }
        }

        go(it2, remainingEvents.drop(windowSize), taken2)
      }

    val initProject = applyVerifiedEventSuccessfully(Project.empty, ves.take(windowSize): _*)
    val init = IssueTracker(initProject)
    go(init, ves.drop(windowSize), windowSize)
  }

  override def tests = Tests {

    'incrementsEqualWhole {

      def test(seed: Long = Gen.long.sample()): Unit = {
        val ves = (Gen.setSeed(seed) >> genEvents).sample()
        for (w <- List(1, 2, 4, 16)) {
          assertIncrementsEqualWhole(w, ves, seed)
        }
      }

      * - test(0)
      * - test()
      * - test()
      * - test()
      * - test()
    }

  }
}
