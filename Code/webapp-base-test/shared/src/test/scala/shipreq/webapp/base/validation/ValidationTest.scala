package shipreq.webapp.base.validation

import japgolly.microlibs.nonempty.NonEmptySet
import utest._
import scalaz.{-\/, Equal, \/, \/-}
import shipreq.base.util.ScalaExt._
import shipreq.base.test.BaseTestUtil._
import shipreq.webapp.base.validation.Simple._
import shipreq.webapp.base.WebappConfig

object ValidationTest extends TestSuite {

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

  override def tests = Tests {

    "common" - {

      "mandatoryShortText" - {
        val test = Tester(CommonValidation.mandatoryShortText.toValidator)
        "1" - test("", "  ", "\n")(fail("blank"))
        "2" - test("hello", " hello ", "\n\nhello \n\n")(pass)
        "3" - test("hello hello", "\n\nhello\n\nhello\n\n")(pass)
        "4" - test("hello great", "hello\n\rgreat")(pass)
        "5" - test("x" * WebappConfig.shortTextMaxLength)(pass)
        "6" - test("x" * (WebappConfig.shortTextMaxLength + 1))(fail("too large"))
      }

      "largeText" - {
        val test = Tester(CommonValidation.largeText.toValidator)
        "1" - test("", "  ", "\n")(pass)
        "2" - test("hello", " hello ", "\n\nhello \n\n")(pass)
        "3" - test("hello\n\ngreat", "\n\nhello\n\ngreat\n \n")(pass)
        "4" - test("x" * WebappConfig.largeTextMaxLength)(pass)
        "5" - test("x" * (WebappConfig.largeTextMaxLength + 1))(fail("too large"))
      }

      "optionalLargeText" - {
        val test = Tester(CommonValidation.optionalLargeText)
        "1" - test(none, "  ", "\n")(fail("blank"))
        "2" - test("hello", " hello ", "\n\nhello \n\n")(pass)
        "3" - test("hello\n\ngreat", "\n\nhello\n\ngreat\n \n")(pass)
        "4" - test("x" * WebappConfig.largeTextMaxLength)(pass)
        "5" - test("x" * (WebappConfig.largeTextMaxLength + 1))(fail("too large"))
      }

      "largeTextSymbols" - {
        def test(input: String, expect: String): Unit =
          assertEq(CommonValidation.largeText.corrector(input), expect)

        test("q>=w && z<=3", "q≥w && z≤3")
        test("q >= w && z <= 3", "q ≥ w && z ≤ 3")
        test(">=w && z<=", "≥w && z≤")
        test("a <=> b", "a <=> b")
        test("a _=> b", "a _=> b")
        test("a >= b >= c", "a ≥ b ≥ c")
      }

      "startsWithRegex" - {
        val test = Tester(CommonValidation.invalidator.startsWithRegex("[0-9]:")(Invalidity("no")).toAuditor.toValidator)
        "pass" - test.each("3:", "8:hehe")(pass)
        "fail" - test.each("3", "33:", ":3")(fail("no"))
      }

      "whitelistCharRangeRegex" - {
        val test = Tester(CommonValidation.invalidator.whitelistCharRangeRegex("0-9")(Invalidity("no")).toAuditor.toValidator)
        "pass" - test.each("", "3", "2342")(pass)
        "fail" - test.each("x3", "3x", "x")(fail("no"))
      }

      "blacklistCharRangeRegex" - {
        val test = Tester(CommonValidation.invalidator.blacklistCharRangeRegex("0-9")(Invalidity("no")).toAuditor.toValidator)
        "pass" - test.each("", "x", "xx sda fjhgkas")(pass)
        "fail" - test.each("3", "x3", "3x", "3x3")(fail("no"))
      }

      "whitelistChars" - {
        val test = Tester(CommonValidation.invalidator.whitelistChars("[0")(Invalidity("no")).toAuditor.toValidator)
        "pass" - test.each("", "0", "[", "[0[[0[")(pass)
        "fail" - test.each("0x", "x[", "3", "!", "[0]")(fail("no"))
      }

      "blacklistChars" - {
        val test = Tester(CommonValidation.invalidator.blacklistChars("[0")(Invalidity("no")).toAuditor.toValidator)
        "pass" - test.each("", "x", "3!")(pass)
        "fail" - test.each("0", "x0", "0x", "[x]")(fail("no"))
      }

      "containsAlphaAndNumber" - {
        val test = Tester(CommonValidation.invalidator.containsAlphaAndNumber.toAuditor.toValidator)
        "pass" - test.each("x3","3x","_x_3_", "_3_x_", "_ 3y3 x6x _")(pass)
        "fail" - test.each("", "_", "3", "x", "__3333_", " x")(fail("contain"))
      }

      "endsWithAlpha" - {
        val test = Tester(CommonValidation.invalidator.endsWithAlpha.toAuditor.toValidator)
        "pass" - test.each("x", "3x", "x  x")(pass)
        "fail" - test.each("", "x3", "3xxx_")(fail("end"))
      }

    } // common

  }
}
