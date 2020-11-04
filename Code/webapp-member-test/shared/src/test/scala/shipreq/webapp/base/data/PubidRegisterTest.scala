package shipreq.webapp.base.data

import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test._
import sourcecode.Line
import utest._

object PubidRegisterTest extends TestSuite {

  override def tests = Tests {

    "uniquePositions" - {
      import PubidRegister.UniquePositions
      import SampleProject.Values._

      def testSome(p: Project, t: ReqTypeId, first: Int, last: Int)(implicit l: Line): Unit =
        assertEq(p.content.reqs.pubids.uniquePositions, Some(UniquePositions(t, first, last)))

      def testNone(p: Project)(implicit l: Line): Unit =
        assertEq(p.content.reqs.pubids.uniquePositions, None)

      "empty"    - testNone(SampleProject.project)
      "biggest1" - testSome(SampleProject2.project, mf, 3, 28)
      "biggest2" - testSome(SampleProject7.project, mf, 4, 28)

      "sole" - {
        val p = applyEventSuccessfully(SampleProject.project, TestEvent.genericReqCreate(GenericReqId(1), fr))
        testSome(p, fr, 1, 1)
      }

      "tie" - {
        val p = applyEventsSuccessfully(SampleProject.project,
          TestEvent.genericReqCreate(GenericReqId(1), fr),
          TestEvent.genericReqCreate(GenericReqId(2), mf),
        )
        testNone(p)
      }
    }

  }
}
