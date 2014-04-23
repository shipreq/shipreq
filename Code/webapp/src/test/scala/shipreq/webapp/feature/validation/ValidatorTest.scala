package shipreq.webapp
package feature.validation

import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.scalatest.prop._
import scalaz.{Failure, Success}
import app.AppConfig._
import lib.Types._

class ValidatorTest extends FunSuite with Matchers with PropertyChecks {
  def V = Validator

  def testV(v: Validator[String, String, String], examples: TableFor2[Option[String], String]): Unit =
    forAll(examples) ((expectedFailure, input) => testV(v, input, expectedFailure))

  def testV(v: Validator[String, String, String], input: String, expectedFailure: Option[String]): Unit =
    v.validate(input.tag) match {
      case Failure(f) => f.toText should include(expectedFailure.getOrElse("Validation failed but was expected to pass."))
      case Success(_) => expectedFailure shouldBe None
    }

  def testCV(v: Validator[String, String, String], examples: TableFor3[String, Option[String], Option[String]]): Unit =
    forAll(examples)((i, cc, expectedFailure) => {
      val c = cc.getOrElse(i)
      v.correct(i) shouldBe c
      testV(v, c, expectedFailure)
    })

  test("Email correction") {
    V.email.correct("hehe") shouldBe "hehe"
    V.email.correct(" he  he ") shouldBe "hehe" // removes ALL whitespace
  }

  test("Email validation") {
    testV(V.email, Table(("Failure Frag", "Input")
      , (None, "hehe@asd.com")
      , (None, "ffs+yay@gmail.com")
      , (Some("invalid"), "heheasd.com")
      , (Some("invalid"), "hehe@asdcom")
      , (Some("invalid"), "hehe@.com")
      , (Some("invalid"), "hehe@asd.")
      , (Some("invalid"), "h&ehe@asd.com")
      , (Some("invalid"), "<hehe@asd.com")
      , (Some("invalid"), ">hehe@asd.com")
      , (Some("invalid"), "hehe@as&d.com")
      , (Some("invalid"), "hehe@as<d.com")
      , (Some("invalid"), "hehe@as>d.com")
    ))
  }

  test("Password validation") {
    testV(V.password, Table(("Failure Frag", "Input")
      , (None, "abc12345")
      , (None, "abc12345" * 10)
      , (None, "12345678a")
      , (None, "a23456789")
      , (None, "1234a6789")
      , (None, "1bcdefghi")
      , (None, "abcd1fghi")
      , (None, "abcdefgh1")
      , (None, "___a__9__")
      , (None, "___9__a__")
      , (None, "a_______9")
      , (None, "9_______a")
      , (None, "@#$%::P1_") // deepti "contributes"
      , (Some(" long."), "")
      , (Some(" long."), "abc456")
      , (Some(" long."), "abc4567")
      , (Some(" long."), "a" + "1" * 128)
      , (Some("at least"), "123456789") // no alpha
      , (Some("at least"), "abcdefghi") // no numbers
      , (Some("at least"), "a________") // no numbers
      , (Some("at least"), "9________") // no alpha
    ))
  }

  test("Password pair validation") {
    Validator.passwords.correctAndValidate("qweqwe123", "qweqwe123h").isFailure shouldBe true
    Validator.passwords.correctAndValidate("qweqwe123", "qweqwe123").isFailure shouldBe false
  }

  test("Username correction") {
    V.user.username.correct("HEHE") shouldBe "hehe"
    V.user.username.correct("  ahah  ") shouldBe "ahah"
    V.user.username.correct("  Heh  ") shouldBe "heh"
  }

  test("Username validation") {
    testV(V.user.username, Table(("Failure Frag", "Input")
      , (None, "abc")
      , (None, "a" * 32)
      , (Some("can only contain"), "@#$%::P1_")
      , (Some(" long."), "")
      , (Some(" long."), "ab")
      , (Some(" long."), "a" * 33)
    ))
  }

  test("UseCaseTitle validation") {
    testV(V.usecase.title, Table(("Failure Frag", "Input")
      , (None, "hello")
      , (None, "hello >")
      , (None, "hello -")
      , (None, "hello --")
      , (None, "hello (again)")
      , (Some("blank"), "")
      , (Some("square"), "hehe [")
      , (Some("square"), "hehe ]")
      , (Some("arrow"), "hehe <--")
      , (Some("arrow"), "hehe ⬅")
      , (Some("arrow"), "hehe ➡")
    ))
  }

  test("MandatoryShortText") {
    testCV(V.project.name, Table(("IN", "CORRECTED", "FAILURE")
      , ("", None, Some("blank"))
      , ("  ", Some(""), Some("blank"))
      , ("hello", None, None)
      , (" hello ", Some("hello"), None)
      , ("\n\nhello\n\n", Some("hello"), None)
      , ("\n\nhello\n\nhello\n\n", Some("hello hello"), None)
      , ("hello\n\rgreat", Some("hello great"), None)
      , ("x" * ShortTextMaxLength, None, None)
      , ("x" * (ShortTextMaxLength + 1), None, Some("too large"))
    ))
  }

  test("LargeText") {
    testCV(V.usecase.textFieldText, Table(("IN", "CORRECTED", "FAILURE")
      , ("", None, None)
      , ("  ", Some(""), None)
      , ("hello", None, None)
      , (" hello ", Some("hello"), None)
      , ("\n\nhello\n\n", Some("hello"), None)
      , ("\n\nhello\n\nhello\n\n", Some("hello\n\nhello"), None)
      , ("x" * LargeTextMaxLength, None, None)
      , ("x" * (LargeTextMaxLength + 1), None, Some("too large"))
    ))
  }

  test("LargeTextO") {
    V.share.preface.correct("\n\n  ") shouldBe None
    V.share.preface.correctAndValidate("") shouldBe Success(None)
    V.share.preface.correctAndValidate("\n\nyo\n\nhehe\n\n") shouldBe Success(Some("yo\n\nhehe".tag))
    V.share.preface.correctAndValidate("x" * LargeTextMaxLength).isSuccess shouldBe true
    V.share.preface.correctAndValidate("x" * (LargeTextMaxLength + 1)).isSuccess shouldBe false
  }

  test("Landing page name") {
    testCV(V.landingPage.name, Table(("IN", "CORRECTED", "FAILURE")
      , (" ", Some(""), Some("blank"))
      , ("Blah", None, Some("surname"))
      , ("Blah Yay5", None, Some("numbers"))
      , ("Blah Yay", None, None)
      , ("Blah Yay Go", None, None)
      , ("Blah Yay-Go", None, None)
      , ("Blah   Yay", Some("Blah Yay"), None)
    ))
  }
}
