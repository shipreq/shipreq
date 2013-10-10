package com.beardedlogic.usecase
package lib

import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.scalatest.prop._
import scalaz.{-\/, \/-}
import Types._

class ValidateTest extends FunSuite with Matchers with PropertyChecks {
  def IV = InputValidator

  test("Validators should prefix failure message with the field name") {
    IV.password.correctAndValidate("").swap.toOption.get should startWith("Password must")
    IV.username.correctAndValidate("").swap.toOption.get should (startWith("Username can only") or startWith("Username must"))
  }

  def testV(iv: InputValidator[String], examples: TableFor2[Option[String], String]) {
    forAll(examples) {
      (failureFrag, input) =>
        iv.validate(input.tag[InputCorrected]) match {
          case -\/(e) => e should include(failureFrag.getOrElse("Validation failed but was expected to pass."))
          case \/-(_) => failureFrag shouldBe None
        }
    }
  }

  test("Email correction") {
    IV.email.correct("hehe") shouldBe "hehe"
    IV.email.correct(" he  he ") shouldBe "hehe" // removes ALL whitespace
  }

  test("Email validation") {
    testV(IV.email, Table(("Failure Frag", "Input")
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
    testV(IV.password, Table(("Failure Frag", "Input")
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

  test("Username correction") {
    IV.username.correct("HEHE") shouldBe "hehe"
    IV.username.correct("  ahah  ") shouldBe "ahah"
    IV.username.correct("  Heh  ") shouldBe "heh"
  }

  test("Username validation") {
    testV(IV.username, Table(("Failure Frag", "Input")
      , (None, "abc")
      , (None, "a" * 32)
      , (Some("can only contain"), "@#$%::P1_")
      , (Some(" long."), "")
      , (Some(" long."), "ab")
      , (Some(" long."), "a" * 33)
    ))
  }

  test("UseCaseTitle validation") {
    testV(IV.useCaseTitle, Table(("Failure Frag", "Input")
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
}
