package com.beardedlogic.usecase.feature.validation

import org.scalatest.{Matchers, FunSuite}
import com.beardedlogic.usecase.app.AppConfig._
import Constraints._

class ConstraintTest extends FunSuite with Matchers {

  def test(i: String, expectPass: Boolean)(implicit c: Constraint[String]): Unit = {
    c.isValid(i) shouldBe expectPass
    c(i).isEmpty shouldBe expectPass
  }
  def pass(i: String)(implicit c: Constraint[String]): Unit = test(i, true)
  def fail(i: String)(implicit c: Constraint[String]): Unit = test(i, false)

  test("Whitelist.chars") {
    implicit val c = Whitelist.chars("a[b", "!")
    pass("")
    pass("[")
    pass("aaa")
    pass("b[ba")
    fail("b[b]a")
    fail(" ")
    fail("!")
  }

  test("Whitelist.charRegex") {
    implicit val c = Whitelist.charRegex("0-5", "!")
    pass("")
    pass("0")
    pass("321")
    fail(" ")
    fail("6")
    fail("32156")
  }

  test("Blacklist.chars") {
    implicit val c = Blacklist.chars("a[b", "!")
    pass("")
    pass(" ")
    pass("hehehe!]")
    fail("[")
    fail("a")
    fail("b")
    fail("heheh[hehe")
  }

  test("Blacklist.charRegex") {
    implicit val c = Blacklist.charRegex("0-5", "!")
    pass("")
    pass(" ")
    pass("hehehe!]")
    fail("3")
    fail("heheh1hehe")
  }

  test("HasLengthInRange") {
    implicit val c = HasLengthInRange(2 to 4)
    fail("")
    fail("1")
    pass("12")
    pass("123")
    pass("1234")
    fail("12345")
  }

  test("NonEmpty") {
    implicit val c = NonEmpty
    fail("")
    pass("1")
    pass("12345")
  }

  test("HasLargeTextLimit") {
    implicit val c = HasLargeTextLimit
    pass("")
    pass("." * (LargeTextMaxLength / 2))
    pass("." * LargeTextMaxLength)
    fail("." * (LargeTextMaxLength + 1))
    fail("." * (LargeTextMaxLength * 2))
    c("." * (LargeTextMaxLength + 666)).get should include(" 666 ")
  }

}
