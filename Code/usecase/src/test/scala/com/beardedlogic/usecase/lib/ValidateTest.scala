package com.beardedlogic.usecase.lib

import org.scalatest.FunSuite
import org.scalatest.Matchers
import org.scalatest.prop._

class ValidateTest extends FunSuite with Matchers with PropertyChecks {

  test("Validators should prefix failure message with the field name") {
    Validate.password("").get should startWith("Password must")
    Validate.username("").get should (startWith("Username can only") or startWith("Username must"))
  }

  def test(validator: Validator[String], examples: TableFor2[Option[String], String]) {
    forAll(examples) {
      (failureFrag, input) =>
        val x = validator(input)
        if (failureFrag.isEmpty)
          x should be(None)
        else {
          x should not be (None)
          x.get should include(failureFrag.get)
        }
    }
  }

  test("#email") {
    test(Validate.email, Table(("Failure Frag", "Input")
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

  test("#password") {
    test(Validate.password, Table(("Failure Frag", "Input")
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

  test("#username") {
    test(Validate.username, Table(("Failure Frag", "Input")
      , (None, "abc")
      , (None, "a" * 32)
      , (Some("can only contain"), "@#$%::P1_")
      , (Some(" long."), "")
      , (Some(" long."), "ab")
      , (Some(" long."), "a" * 33)
    ))
  }
}
