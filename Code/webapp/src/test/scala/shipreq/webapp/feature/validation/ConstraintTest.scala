package shipreq.webapp.feature.validation

import javax.script.ScriptEngineManager
import org.apache.commons.lang3.StringEscapeUtils
import org.scalatest.{Matchers, FunSuite}
import org.scalatest.prop.PropertyChecks
import shipreq.webapp.app.AppConfig._
import shipreq.webapp.test.TestHelpers.Cores
import Constraint.{not => NOT}
import Constraints._

class ConstraintTest extends FunSuite with Matchers with PropertyChecks {

  implicit override val generatorDrivenConfig = PropertyCheckConfig(minSuccessful = Cores * 100, workers = Cores)

  val engine = new ScriptEngineManager(null).getEngineByExtension("js").ensuring(_ != null, "JavaScript engine unavailable")

  def test(i: String, expectPass: Boolean)(implicit c: Constraint[String]): Unit = {
    c.isValid(i) shouldBe expectPass
    c.invalidate(i).isEmpty shouldBe expectPass
    testJs(c, i)
  }
  def valid(i: String)(implicit c: Constraint[String]): Unit = test(i, true)
  def invalid(i: String)(implicit c: Constraint[String]): Unit = test(i, false)

  def testJs(c: Constraint[String], v: String): Unit = {
    val expected = c.isValid(v)
    val js1 = s"(function(_){return ${c.js}})"
    val js2 = s"""("${StringEscapeUtils escapeEcmaScript v}")"""
    val js = js1 + js2
    val o = engine.eval(js)
    val result: Boolean = o.asInstanceOf[java.lang.Boolean]
    if (result != expected) {
      println(c.js)
      println(debugStr(c.js))
      fail(s"""F: ${c.js} | $result shouldBe $expected""")
    }
    result shouldEqual expected
  }

  def debugStr(s: String) = s.toCharArray.toList.map(c => s"${c.toInt}/$c")

  test("matchesR.js") {
    forAll { (ac: Char, aa: String, b: String) =>
      val a = if (aa.isEmpty) ac.toString else aa
      val c = whitelistCharsS(a)("!")
      testJs(c, b)
    }
  }

  test("whitelistCharsS") {
    implicit val c = whitelistCharsS("a][b")("!")
    valid("")
    valid("[")
    valid("]")
    valid("aaa")
    valid("b[ba")
    valid("b[b]a")
    invalid("b[b]ac")
    invalid(" ")
    invalid("!")
  }

  test("whitelistCharsR") {
    implicit val c = whitelistCharsR("0-5")("!")
    valid("")
    valid("0")
    valid("321")
    invalid(" ")
    invalid("6")
    invalid("32156")
  }

  test("blacklistCharsS") {
    implicit val c = blacklistCharsS("a[b")("!")
    valid("")
    valid(" ")
    valid("hehehe!]")
    invalid("[")
    invalid("a")
    invalid("b")
    invalid("heheh[hehe")
  }

  test("blacklistCharsR") {
    implicit val c = blacklistCharsR("0-5")("!")
    valid("")
    valid(" ")
    valid("hehehe!]")
    invalid("3")
    invalid("heheh1hehe")
  }

  test("lengthInRange") {
    implicit val c = lengthInRange(2 to 4)
    invalid("")
    invalid("1")
    valid("12")
    valid("123")
    valid("1234")
    invalid("12345")
  }

  test("nonEmpty") {
    implicit val c = nonEmpty
    invalid("")
    valid("1")
    valid("12345")
  }

  test("largeTextLimit") {
    implicit val c = largeTextLimit
    valid("")
    valid("." * (LargeTextMaxLength / 2))
    valid("." * LargeTextMaxLength)
    invalid("." * (LargeTextMaxLength + 1))
    invalid("." * (LargeTextMaxLength * 2))
    c.invalidate("." * (LargeTextMaxLength + 666)).head should include(" 666 ")
  }

  test("containsSurname") {
    implicit val c = containsSurname
    invalid("")
    invalid("a")
    invalid("abc")
    valid("abc abc")
    valid("B B")
    valid(" abc  abc ")
    valid(" abc  def qwe asdf")
    c.invalidate("").head shouldNot include("name")
    c.invalidate("firstOnly").head should include("name")
  }

  test("not(matchesR)") {
    implicit val c = NOT(matchesR("[0-9]+".r))("good")
    invalid("123")
    valid("yay")
  }
}
