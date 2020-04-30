package shipreq.webapp.base.data.derivation

import scalaz.{-\/, \/-}
import sourcecode.Line
import utest._
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.data._
import shipreq.webapp.base.test.SampleProject6
import shipreq.webapp.base.test.UnsafeTypes._

object UseCaseStepLabelLookupTest extends TestSuite {
  import UseCaseStepLabelLookup._

  private def test(label: String, expect: Result, allowAliases: Boolean = true)
                  (implicit q: Line, pos: ReqTypePos, ll: UseCaseStepLabelLookup): Unit =
    for (l <- label.toLowerCase :: label.toUpperCase :: Nil)
      assertEq(l, ll(pos, l, allowAliases), expect)

  private def testOk(id: UseCaseStepId, label: String)
                    (implicit l: Line, pos: ReqTypePos, ll: UseCaseStepLabelLookup): Unit =
    test(label, \/-(id))

  private def testNotFound(label: String)
                          (implicit l: Line, pos: ReqTypePos, ll: UseCaseStepLabelLookup): Unit =
    test(label, -\/(Failure.NotFound))

  private def testAmbiguous(label: String)(one: String, two: String, others: String*)
                           (implicit l: Line, pos: ReqTypePos, ll: UseCaseStepLabelLookup): Unit =
    test(label, -\/(Failure.Ambiguous(one, two, others.toSet)))

  override def tests = Tests {

    "sp6" - {
      implicit val ll = SampleProject6.project.content.reqs.useCaseStepLabelLookup
      implicit val pos1 = ReqTypePos(1)

      "unique" - {
        testOk(10, "1.0")
        testOk(11, "1.0.1")
        testOk(12, "1.0.2")
        testOk(19, "1.0.2.a")
        testOk(13, "1.0.3")
        testOk(14, "1.1")
        testOk(15, "1.1.1")
        testOk(18, "1.e.1")
        testOk(16, "1.0.x.1")
        testOk(17, "1.e.x.1")
        testOk(20, "1.x.0")
      }

      "aliases" - {
        testOk(11, ".0.1")
        testOk(12, "102")
        testOk(12, "1.02")
        testOk(12, "02")
        testOk(12, "2")
        testOk(12, ".2")
        testOk(12, "0.2")
        testOk(12, ".0.2")
        testOk(12, ".02")
        testOk(18, "1e1")
        testOk(18, "e1")
        testOk(15, "111")
        testOk(16, "0x1")
        testOk(19, "a")
        testOk(19, ".a")
        testOk(19, "2a")
        testOk(19, "2.a")
        testOk(19, ".2.a")
        testOk(19, ".2a")
      }

      "notFound" - {
        testNotFound("102.")
        testNotFound("1.0.2.")
        testNotFound(".1.0.2")
        testNotFound(".102")
      }

      "ambiguous" - {
        testAmbiguous("11")("1.1.1", "1.1")
        testAmbiguous("x1")("1.E.X.1", "1.0.X.1")
      }

      "extraZeros" - {
        "unique" - {
          testOk(11, "1.00.1")
          testOk(11, "1.00.01")
          testOk(19, "0001.000.0002.a")
          testNotFound("1.0.2.a0")
          testNotFound("1.0.2.0a")
          testNotFound("1.0.2.a0")
          testNotFound("1.0.20.a")
        }

        "aliases" - {
          testNotFound("1002")
          testNotFound("1.002")
          testNotFound(".0a")
          testNotFound(".a0")
          testOk(12, ".0000000000000002")
        }
      }
    }
  }
}
