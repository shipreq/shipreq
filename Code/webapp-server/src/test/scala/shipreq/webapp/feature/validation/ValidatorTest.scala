package shipreq.webapp.feature.validation

import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.scalatest.prop._
import scalaz.{Failure, Success}
import shipreq.webapp.base.AppConsts._
import shipreq.webapp.base.validation._
import shipreq.webapp.security.PasswordAndSalt

// TODO Move ValidatorTest into webapp-shared

class ValidatorTest extends FunSuite with Matchers with PropertyChecks {
  def V = Validators

  type VT3 = ValidatorU[String, String, String]

  def testV(v: VT3, examples: TableFor2[Option[String], String]): Unit =
    forAll(examples) ((expectedFailure, input) => testV(v, input, expectedFailure))

  def testV(v: VT3, input: String, expectedFailure: Option[String]): Unit =
    v.validateU(InputCorrected(input)) match {
      case Failure(f) => f.toText should include(expectedFailure.getOrElse("Validation failed but was expected to pass."))
      case Success(_) => expectedFailure shouldBe None
    }

  def testCV(v: VT3, examples: TableFor3[String, Option[String], Option[String]]): Unit =
    forAll(examples)((i, cc, expectedFailure) => {
      val c = cc.getOrElse(i)
      v.correctU(i) shouldBe c
      testV(v, c, expectedFailure)
    })

  test("Email correction") {
    V.email.correctU("hehe") shouldBe "hehe"
    V.email.correctU(" he  he ") shouldBe "hehe" // removes ALL whitespace
  }

  test("Email validation") {
    testV(V.email_, Table(("Failure Frag", "Input")
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
    Validators.passwords.correctAndValidateU("qweqwe123", "qweqwe123h").isFailure shouldBe true
    Validators.passwords.correctAndValidateU("qweqwe123", "qweqwe123").isFailure shouldBe false
  }

  test("Password change") {
    val ps = PasswordAndSalt.createWithRandomSalt("blahblah8")
    val v = Validators.passwordChange(ps)
    v.correctAndValidateU("blahblah", ("qweqwe123", "qweqwe123")).isFailure shouldBe true
    v.correctAndValidateU("blahblah8", ("qweqwe12", "qweqwe123")).isFailure shouldBe true
    v.correctAndValidateU("blahblah8", ("qweqwe123", "")).isFailure shouldBe true
    v.correctAndValidateU("blahblah8", ("qweqwe123", "qweqwe123")) shouldBe Success("qweqwe123")
  }

  test("Password set") {
    val v = Validators.passwordSet
    v.correctAndValidateU("", ("qweqwe12", "qweqwe123")).isFailure shouldBe true
    v.correctAndValidateU("", ("qweqwe123", "")).isFailure shouldBe true
    v.correctAndValidateU("", ("qweqwe123", "qweqwe123")) shouldBe Success("qweqwe123")
  }

  test("Username correction") {
    V.user.username.correctU("HEHE")     shouldBe "hehe"
    V.user.username.correctU("  ahah  ") shouldBe "ahah"
    V.user.username.correctU("  Heh  ")  shouldBe "heh"
  }

  test("Username validation") {
    testV(V.user.username_, Table(("Failure Frag", "Input")
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
      , ("x" * shortTextMaxLength, None, None)
      , ("x" * (shortTextMaxLength + 1), None, Some("too large"))
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
      , ("x" * largeTextMaxLength, None, None)
      , ("x" * (largeTextMaxLength + 1), None, Some("too large"))
    ))
  }

  test("LargeTextO") {
    V.share.preface.correctU("\n\n  ") shouldBe None
    V.share.preface.correctAndValidateU("") shouldBe Success(None)
    V.share.preface.correctAndValidateU("\n\nyo\n\nhehe\n\n") shouldBe Success(Some("yo\n\nhehe"))
    V.share.preface.correctAndValidateU("x" * largeTextMaxLength).isSuccess shouldBe true
    V.share.preface.correctAndValidateU("x" * (largeTextMaxLength + 1)).isSuccess shouldBe false
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
