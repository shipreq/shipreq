package shipreq.webapp.base.data

import utest._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.test.UnsafeTypes._

object DataTest extends TestSuite {

  import Validators.shared._

  @inline def tr(a: Option[TagId], b: HashRefKey) = (a,b)
  @inline def ir(a: Option[CustomIssueTypeId], b: HashRefKey) = (a,b)

  val tagData = Stream(tr(1.AT, "abc"), tr(2.AT, "def"))
  val issueData = Stream(ir(1, "tbd"), ir(3, "todo"))

  override def tests = TestSuite {
    'validation {
      'hashRefKeyUniqueness {

        def test(input: String, expectValid: Boolean, subjT: Option[TagId] = None, subjI: Option[CustomIssueTypeId] = None): Unit = {
          val vs = HashRefKeyVS((subjT, tagData), (subjI, issueData))
          assertEq(s"[$input] | $subjT, $subjI", hashRefKeyS.isValid(vs, input), expectValid)
        }

        'preventDups {
          test("hehe", true)
          test("abc", false)
          test("todo", false)
          test("   todo   ", false)
        }

        'subjCanChangeItself {
          test("abc", true, subjT = 1.AT)
          test("abc", false, subjT = 2.AT)
          test("todo", true, subjI = 3)
          test("todo", false, subjI = 1)
        }

        'caseInsensitive {
          test("ABC", false)
          test("ABCD", true)
          test("ABC", true, subjT = 1.AT)
          test("ABC", false, subjT = 2.AT)
          test("ABC", false, subjT = 3.AT)
        }
      }
    }
  }
}