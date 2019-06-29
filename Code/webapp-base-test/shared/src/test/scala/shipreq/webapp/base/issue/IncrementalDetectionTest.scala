package shipreq.webapp.base.issue

import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.Duration
import nyaya.gen.Gen
import scala.annotation.tailrec
import utest._
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.{EventSeqSummary, RandomEventStream, VerifiedEvent}
import shipreq.webapp.base.test.WebappTestUtil._

object IncrementalDetectionTest extends TestSuite {

  private def eventsStreamSize = 1024
  private def compareIncTime = true
  private var fullDurs, incDurs = List.empty[Duration]

  private def logCmpDur(desc: String, full: Duration, inc: Duration): Unit = {
    val saved = full.minus(inc)
    printf(s"%-10s :   init = %8s,   inc = %8s,   saved = %10s\n", desc, full.conciseDesc, inc.conciseDesc, saved.conciseDesc)
  }

  private val genEvents = RandomEventStream.justEntireEventStream(eventsStreamSize)

  private def assertIncrementsEqualWhole(windowSize: Int, ves: Vector[VerifiedEvent], seed: Long): Unit = {
    @tailrec
    def go(it: IssueTracker, remainingEvents: Vector[VerifiedEvent], taken: Int): Unit =
      if (remainingEvents.nonEmpty) {
        val es        = remainingEvents.take(windowSize)
        val p2        = applyVerifiedEventSuccessfully(it.project, es: _*)
        val incStart  = System.nanoTime()
        val it2       = it.update(es.iterator.map(_.event), p2)
        val incEnd    = System.nanoTime()
        val taken2    = taken + windowSize
        val fullStart = System.nanoTime()
        val expect    = IssueTracker(p2)
        val fullEnd   = System.nanoTime()

        if (compareIncTime) {
          val fullDur = Duration.ofNanos(fullEnd - fullStart)
          val incDur  = Duration.ofNanos(incEnd - incStart)
          fullDurs  ::= fullDur
          incDurs   ::= incDur
          logCmpDur(s"$taken2+$windowSize", full = fullDur, inc = incDur)
        }

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
          for (i <- taken until taken2) {
            val e = ves(i)
            printf("%2d: %s\n", i, e.event.toString.replaceAll("[^ -z]+", " ").take(180))
          }
          println()
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
        for (w <- List(2)) {
          assertIncrementsEqualWhole(w, ves, seed)
        }
      }

//>>>>>>>>> [windowSize=1, seed = -3431641283354444567L , events=257]
// missing: ConflictingTags(UseCaseId(2),TagGroupId(12),NonEmptySet(CustomTextField(CustomField.Text.Id(9)), CustomTextField(CustomField.Text.Id(8))))
//          ConflictingTags(UseCaseId(2),TagGroupId(22),NonEmptySet(CustomTextField(CustomField.Text.Id(9)), CustomTextField(CustomField.Text.Id(8))))
//          ConflictingTags(UseCaseId(2),TagGroupId(30),NonEmptySet(CustomTextField(CustomField.Text.Id(9)), CustomTextField(CustomField.Text.Id(8))))
//unwanted: ConflictingTags(UseCaseId(2),TagGroupId(12),NonEmptySet(CustomTextField(CustomField.Text.Id(8)), CustomTextField(CustomField.Text.Id(9))))
//          ConflictingTags(UseCaseId(2),TagGroupId(22),NonEmptySet(CustomTextField(CustomField.Text.Id(8)), CustomTextField(CustomField.Text.Id(9))))
//          ConflictingTags(UseCaseId(2),TagGroupId(30),NonEmptySet(CustomTextField(CustomField.Text.Id(8)), CustomTextField(CustomField.Text.Id(9))))
//EventSeqSummary(
//  customIssueTypes     = CUDR(C={}, U={}, D={}, R={})
//  customFieldImpTypes  = CUDR(C={}, U={}, D={}, R={})
//  customFieldTagTypes  = CUDR(C={}, U={}, D={}, R={})
//  customFieldTextTypes = CUDR(C={}, U={}, D={}, R={})
//  customReqTypes       = CUDR(C={}, U={}, D={}, R={})
//  tagGroups            = CUDR(C={}, U={}, D={}, R={})
//  applicableTags       = CUDR(C={}, U={}, D={}, R={})
//  reqCodeGroups        = CUDR(C={}, U={}, D={}, R={})
//  staticFields         = CUDR(C={}, U={}, D={}, R={})
//  genericReqs          = CUDR(C={}, U={5}, D={}, R={})
//  useCasesExclSteps    = CUDR(C={}, U={}, D={}, R={})
//  useCaseSteps         = CUDR(C={}, U={}, D={}, R={})
//  apReqCodes           = false
//  contentLiveDeps      = false ){
//  hasTags              = false }
//
//257: ReqFieldCustomTextSet(GenericReqId(5),CustomField.Text.Id(8),Vector(Issue(CustomIssueTypeId(3),Vector(EmailAddress($gX3quQQB%@*sd.i44cfliN.cDc6c%Ma1waG_.9Uv.lHL ), MathTeX(l P M *

//      * - test(3363882791775821664L)
      * - test()
      * - test()
      * - test()
      * - test()
      * - test()
      * - test()
      * - test()
      * - test()
      * - test()
      * - test()
    }
  }

  override def utestAfterAll(): Unit = {
    if (compareIncTime) {
      val samples = fullDurs.size
      val List(fullTotal, incTotal) = List(fullDurs, incDurs).map(_.foldLeft(Duration.ZERO)(_ plus _))
      val List(fullAvg, incAvg)     = List(fullTotal, incTotal).map(_.dividedBy(samples))

      logCmpDur("AVERAGE", full = fullAvg, inc = incAvg)
      logCmpDur("TOTAL", full = fullTotal, inc = incTotal)
    }
  }
}
