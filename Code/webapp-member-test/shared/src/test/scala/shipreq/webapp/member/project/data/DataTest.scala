package shipreq.webapp.member.project.data

import shipreq.base.util._
import shipreq.webapp.member.test.WebappTestUtil._
import shipreq.webapp.member.test.project.UnsafeTypes._
import utest._

object DataTest extends TestSuite {

  @inline def tr(a: Option[TagId], b: HashRefKey) = (a,b)
  @inline def ir(a: Option[CustomIssueTypeId], b: HashRefKey) = (a,b)

  val tagData = List(tr(1.AT, "abc"), tr(2.AT, "def"))
  val issueData = List(ir(1, "tbd"), ir(3, "todo"))

  override def tests = Tests {
    "validation" - {
      "hashRefKeyUniqueness" - {
        import DataValidators.hashRefKey._

        def test(input: String, expectedValidity: Validity, subjT: Option[TagId] = None, subjI: Option[CustomIssueTypeId] = None): Unit = {
          val state = State(SubState(subjT, () => tagData), SubState(subjI, () => issueData))
          assertEq(s"[$input] | $subjT, $subjI", hashRefKey(state).validity(input), expectedValidity)
        }

        "preventDups" - {
          test("hehe", Valid)
          test("abc", Invalid)
          test("todo", Invalid)
          test("   todo   ", Invalid)
        }

        "subjCanChangeItself" - {
          test("abc", Valid, subjT = 1.AT)
          test("abc", Invalid, subjT = 2.AT)
          test("todo", Valid, subjI = 3)
          test("todo", Invalid, subjI = 1)
        }

        "caseInsensitive" - {
          test("ABC", Invalid)
          test("ABCD", Valid)
          test("ABC", Valid, subjT = 1.AT)
          test("ABC", Invalid, subjT = 2.AT)
          test("ABC", Invalid, subjT = 3.AT)
        }
      }
    }
  }
}