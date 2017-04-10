package shipreq.webapp.server.feature.validation

import japgolly.microlibs.nonempty.NonEmptySet
import utest._
import scalaz.{-\/, Equal, \/, \/-}
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.{Invalid, Valid}
import shipreq.webapp.base.vali2._
import shipreq.webapp.server.security.PasswordAndSalt
import Simple._

object ValidatorTest extends TestSuite {

  case class Tester[I, C: Equal, V: Equal](v: Validator[I, C, V]) {

    def each(correct1: C, correctN: C*)(expectResult: C => String \/ V): Unit =
      for (c <- correct1 +: correctN)
        apply(c)(expectResult)

    def apply(correct: C, inputs: I*)(expectResult: C => String \/ V): Unit = {

      // Test inputs
      for (i <- inputs)
        assertEq(s"full $$ $i", v.corrector(i), correct)

      // Test uncorrect
      val uncorrected = v.corrector.uncorrect(correct)
      assertEq("full.uncorrect.full = full", v.corrector(uncorrected), correct)

      // Test validation
      val result = v(uncorrected)
      expectResult(correct) match {
        case r@ \/-(_) =>
          assertEq(s"Result of '$uncorrected'", result, r)
        case -\/(expect) =>
          result match {
            case -\/(NonEmptySet.Sole(e)) => assertContainsCI(e, expect)
            case _ => fail(s"One error expected; got: $result")
          }
      }
    }
  }

  def pass[A](a: A) = \/-(a)
  def fail(e: String) = (_: Any) => -\/(e)
  implicit def someString(e: String) = Option(e)

  override def tests = TestSuite {

    'email {
      val test = Tester(ServerSideValidators.email.unnamed.mapValid(_.value))

      'correction {
        assertEq(test.v.corrector(" he   he  "), "hehe") // removes ALL whitespace
      }
      'plain - test("hehe@asd.com")(pass)
      'plus - test("ffs+yay@gmail.com")(pass)
      'invalid - test.each(
          "heheasd.com",
          "hehe@asdcom",
          "hehe@.com",
          "hehe@asd.",
          "h&ehe@asd.com",
          "<hehe@asd.com",
          ">hehe@asd.com",
          "hehe@as&d.com",
          "hehe@as<d.com",
          "hehe@as>d.com")(fail("invalid"))
    }

    'password {
      val test = Tester(ServerSideValidators.password.unnamed)
      * - test("abc12345")(pass)
      * - test("abc12345" * 10)(pass)
      * - test("12345678a")(pass)
      * - test("a23456789")(pass)
      * - test("1234a6789")(pass)
      * - test("1bcdefghi")(pass)
      * - test("abcd1fghi")(pass)
      * - test("abcdefgh1")(pass)
      * - test("___a__9__")(pass)
      * - test("___9__a__")(pass)
      * - test("a_______9")(pass)
      * - test("9_______a")(pass)
      * - test("@#$%::P1_")(pass) // deepti contributes
      * - test("")(fail(" long."))
      * - test("abc456")(fail(" long."))
      * - test("abc4567")(fail(" long."))
      * - test("123456789")(fail("at least")) // no alpha
      * - test("abcdefghi")(fail("at least")) // no numbers
      * - test("a________")(fail("at least")) // no numbers
      * - test("9________")(fail("at least")) // no alpha
      * - assertEq(test.v.auditor.validity("a" + "1" * 128), Invalid) // too long
    }

    'passwordTwice {
      'diff - assertEq(ServerSideValidators.passwordTwice.validity(("qweqwe123", "qweqwe123h")), Invalid)
      'same - assertEq(ServerSideValidators.passwordTwice.validity(("qweqwe123", "qweqwe123")), Valid)
    }

    'passwordChange {
      val ps = PasswordAndSalt.createWithRandomSalt("blahblah8")
      val v = ServerSideValidators.passwordChange(ps)
      * - assertEq(v.validity(("blahblah", ("qweqwe123", "qweqwe123"))), Invalid)
      * - assertEq(v.validity(("blahblah8", ("qweqwe12", "qweqwe123"))), Invalid)
      * - assertEq(v.validity(("blahblah8", ("qweqwe123", ""))), Invalid)
      * - assertEq(v(("blahblah8", ("qweqwe123", "qweqwe123"))), \/-("qweqwe123"))(Equal.equalA)
    }

    'username {
      val test = Tester(ServerSideValidators.user.username.unnamed.mapValid(_.value))
      * - test("hehe", "HEHE", "  Hehe  ")(pass)
      * - test("a" * 3)(pass)
      * - test("@#$%::p1_")(fail("can only contain"))
      * - test("")(fail(" long."))
      * - test("ab")(fail(" long.")) // too short
      * - assertEq(test.v.auditor.validity("a" * 33), Invalid) // too long
    }

    'landingPageName {
      val test = Tester(ServerSideValidators.landingPage.name.unnamed)
      * - test("", " ")(fail("blank"))
      * - test("Blah")(fail("surname"))
      * - test("Blah Yay5")(fail("numbers"))
      * - test.each("Blah Yay", "Blah Yay Go", "Blah Yay-Go")(pass)
      * - test("Blah Yay", "Blah   Yay ")(pass)
    }

  }
}
