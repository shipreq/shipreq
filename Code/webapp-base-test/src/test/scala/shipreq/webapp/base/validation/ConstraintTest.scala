package shipreq.webapp.base.validation

import utest._
import shipreq.webapp.base.AppConsts._
import Constraint.{not => NOT}
import Constraints._

object ConstraintTest extends TestSuite {
  override def tests = TestSuite {

    def test(i: String, expectPass: Boolean)(implicit c: Constraint[String]): Unit = {
      assert(c.isValid(i) == expectPass)
      assert(c.invalidate(i).isEmpty == expectPass)
    }
    def valid(i: String)(implicit c: Constraint[String]): Unit = test(i, true)
    def invalid(i: String)(implicit c: Constraint[String]): Unit = test(i, false)

    def testm(i: String, frag: String, exp: Boolean = true)(implicit c: Constraint[String]) = {
      val m = c.invalidate(i).head
      assert(m.contains(frag) == exp)
    }

    'whitelistCharsS {
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

    'whitelistCharsR {
      implicit val c = whitelistCharsR("0-5")("!")
      valid("")
      valid("0")
      valid("321")
      invalid(" ")
      invalid("6")
      invalid("32156")
    }

    'blacklistCharsS {
      implicit val c = blacklistCharsS("a[b")("!")
      valid("")
      valid(" ")
      valid("hehehe!]")
      invalid("[")
      invalid("a")
      invalid("b")
      invalid("heheh[hehe")
    }

    'blacklistCharsR {
      implicit val c = blacklistCharsR("0-5")("!")
      valid("")
      valid(" ")
      valid("hehehe!]")
      invalid("3")
      invalid("heheh1hehe")
    }

    'lengthInRange {
      implicit val c = lengthInRange(2 to 4)
      invalid("")
      invalid("1")
      valid("12")
      valid("123")
      valid("1234")
      invalid("12345")
    }

    'nonEmpty {
      implicit val c = nonEmpty
      invalid("")
      valid("1")
      valid("12345")
    }

    'largeTextLimit {
      implicit val c = largeTextLimit
      valid("")
      valid("." * (largeTextMaxLength / 2))
      valid("." * largeTextMaxLength)
      invalid("." * (largeTextMaxLength + 1))
      invalid("." * (largeTextMaxLength * 2))
      testm("." * (largeTextMaxLength + 666), " 666 ")
    }

    'containsSurname {
      implicit val c = containsSurname
      invalid("")
      invalid("a")
      invalid("abc")
      valid("abc abc")
      valid("B B")
      valid(" abc  abc ")
      valid(" abc  def qwe asdf")
      testm("", "name", false)
      testm("firstOnly", "name")
    }

    "not(matchesR)"-{
      implicit val c = NOT(matchesR("[0-9]+".r))("good")
      invalid("123")
      valid("yay")
    }
  }
}
