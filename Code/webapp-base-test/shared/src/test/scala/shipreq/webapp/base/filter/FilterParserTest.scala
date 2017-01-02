package shipreq.webapp.base.filter

import japgolly.microlibs.nonempty._
import nyaya.prop._
import nyaya.test._
import nyaya.test.PropTestOps._
import org.parboiled2.{ErrorFormatter, ParseError}
import scala.util.{Failure, Success}
import utest._
import shipreq.base.util.Debug._
import shipreq.base.util._
import shipreq.webapp.base.{RandomData => $}
import shipreq.webapp.base.data.HashRefKey
import shipreq.webapp.base.data.ReqType.Mnemonic
import shipreq.webapp.base.test.WebappTestUtil._
import FilterSpec._

object FilterParserTest extends TestSuite {

  implicit def equality: UnivEq[FilterSpec] = UnivEq.force

  implicit def autoSome(f: FilterSpec) = Option(f)
  implicit def autoMne(s: String) = Mnemonic(s)
  implicit def autoHRK(s: String) = HashRefKey(s)
  implicit def autoNev[A](a: A) = NonEmptyVector(a)
  implicit def NES[A: UnivEq](a: A, as: A*) = NonEmptySet(a, as.toSet)

  def allOf(a: FilterSpec, b: FilterSpec*) = AllOf(NonEmptyVector(a, b: _*))
  def anyOf(a: FilterSpec, b: FilterSpec*) = AnyOf(NonEmptyVector(a, b: _*))

  val prism = monocle.Prism[String, Option[FilterSpec]](
    new FilterParser(_).main.run().toOption)(_.fold("")(toText))

  val prismFromString =
    Prop.eval[String] { str =>
      val e = EvalOver(str)
      def norm(s: String) = s.filterNot(_.isWhitespace)
      prism.getOrModify(str).toOption match {
        case Some(os) => e.equal("toText . parse = id", norm(prism reverseGet os), norm(str))
        case None     => e.pass
      }
    }

  val prismFromSpec =
    Prop.test[FilterSpec]("prismFromSpec", spec => {
      val s = Some(spec)
      test(prism reverseGet s, s)
      true
    })

  def test(str: String, exp: Option[FilterSpec]): Unit = {
    val p = new FilterParser(str)
    p.main.run() match {
      case Success(a)             => assertEq(s"[$str]", a, exp)
      case Failure(e: ParseError) => println(p.formatError(e, new ErrorFormatter(showTraces = true))); fail("Parser failed.")
      case Failure(e: Throwable)  => fail("Parser failed: " + Option(e.getMessage).getOrElse(e.toString))
    }
  }

  def testFail(str: String): Unit =
    new FilterParser(str).main.run() match {
      case Success(a) => fail(s"[$str] succeeded with $a")
      case Failure(_) => ()
    }

  override def tests = TestSuite {

    'prop {
      import PropTest.defaultPropSettings
//      implicit def settings = DefaultSettings.propSettings.setSampleSize(10000)
      'fromString - prismFromString.mustBeSatisfiedBy($.unicodeString)
      'fromSpec   - prismFromSpec  .mustBeSatisfiedBy($.filter.spec.filterSpec)
    }

    'empty {
      test("", None)
      test("   ", None)
    }

    'simpleText {
      'simple      - test("a",         SimpleText("a"))
      'blurt       - test("weqfd351!", SimpleText("weqfd351!"))
      'singleQuote - test("they're",   SimpleText("they're"))
      'longUpper   - test("Q"*30,      SimpleText("Q"*30))
    }

    'quotedText {
      'empty  - testFail("''")
      'open1  - testFail("'")
      'open2  - testFail("'asdf")
      'open3  - testFail("'asdf`")
      'simple - test("'hehe'", QuotedText("hehe", '\''))
      'qmix1  - test("""'1 ` " 2'""", QuotedText("""1 ` " 2""", '\''))
      'qmix2  - test("""`1 ' " 2`""", QuotedText("""1 ' " 2""", '`'))
      'qmix3  - test(""""1 ' ` 2"""", QuotedText("""1 ' ` 2""", '"'))
    }

    'regex {
      'empty  - testFail("//")
      'open1  - testFail("/")
      'open2  - testFail("/asdf asdf ")
      'simple - test("/a/", Regex("a"))
      'qchars - test("/'.*`.*`'/", Regex("'.*`.*`'"))
      'escape - test("""/\\ \. \d \s \n \/ \W/""", Regex("""\\ \. \d \s \n / \W"""))
    }

    'reqType {
      * - test("X",  ReqType("X"))
      * - test("MF", ReqType("MF"))
      * - test("BOBOP", ReqType("BOBOP"))
    }

    'hashRef {
      'empty1  - testFail("#")
      'empty2  - testFail("# ")
      'simple1 - test("#x",       HashRef("x"))
      'simple2 - test("#pri=123", HashRef("pri=123"))
      'anyCase - test("#TO"+"DO", HashRef("TO"+"DO"))
    }

    'implication {
      'empty1    - testFail("implies:")
      'empty2    - testFail("impliedBy:  ")
      'open1     - testFail("impliedBy:MF-")
      'open2     - testFail("impliedBy:MF-{")
      'open3     - testFail("impliedBy:MF-{3,4")
      'wholeType - test("impliedBy:MF",                  ImpliedBy(WholeType("MF")))
      'single    - test("implies:MF-2",                  Implies  (SomeOfType("MF", NES(2))))
      'specific  - test("impliedBy:CO-{3,5,7}",          ImpliedBy(SomeOfType("CO", NES(3,5,7))))
      'range     - test("implies:CO-{3-6}",              Implies  (SomeOfType("CO", NES(3,4,5,6))))
      'rangeRev  - test("impliedBy:CO-{9-7}",            ImpliedBy(SomeOfType("CO", NES(7,8,9))))
      'combo     - test("implies:SI,DD-{3-5,9,12-14,1}", Implies  (NonEmptyVector(WholeType("SI"), SomeOfType("DD", NES(3,4,5,9,12,13,14,1)))))
      'lower     - test("impliedBy:dgh",                 ImpliedBy(WholeType("DGH")))
    }

    'presence {
      'empty  - testFail("has:")
      'simple - test("has:stuff", Presence("stuff"))
    }

    'lack {
      'empty  - testFail("no:")
      'simple - test("no:stuff", Lack("stuff"))
    }

    'unknownKeys {
      'empty        - testFail(":")
      'typoLack     - testFail("noo:stuff")
      'typoPresence - testFail("hass:stuff")
      'typoImplies  - testFail("iplies:MF")
      'crap         - testFail("crap:")
    }

    'allOf {
      'empty1 - testFail("()")
      'empty2 - testFail("(  )")
      'open1  - testFail("(")
      'open2  - testFail("(MF")
      'one    - test("(MF)",        allOf(ReqType("MF")))
      'oneWS  - test("(   MF  )",   allOf(ReqType("MF")))
      'two    - test("(MF BR)",     allOf(ReqType("MF"), ReqType("BR")))
      'three  - test("(#X #Y yay)", allOf(HashRef("X"), HashRef("Y"), SimpleText("yay")))
      'nest   - test("((MF))",      allOf(allOf(ReqType("MF"))))
    }

    'anyOf {
      'empty1 - testFail("{}")
      'empty2 - testFail("{  }")
      'open1  - testFail("{")
      'open2  - testFail("{MF")
      'one    - test("{MF}",        anyOf(ReqType("MF")))
      'oneWS  - test("{   MF  }",   anyOf(ReqType("MF")))
      'two    - test("{MF BR}",     anyOf(ReqType("MF"), ReqType("BR")))
      'three  - test("{#X #Y yay}", anyOf(HashRef("X"), HashRef("Y"), SimpleText("yay")))
      'nest   - test("{{MF}}",      anyOf(anyOf(ReqType("MF"))))
    }

    'not {
      'empty1     - testFail("-")
      'empty2     - testFail("--")
      'simpleText - test("-car",            Not(SimpleText("car")))
      'quotedText - test("-'eat food'",     Not(QuotedText("eat food", '\'')))
      'regex      - test("-/a .*b/",        Not(Regex     ("a .*b")))
      'reqType    - test("-MF",             Not(ReqType   ("MF")))
      'hashRef    - test("-#boo",           Not(HashRef   ("boo")))
      'implies    - test("-implies:XYZ",    Not(Implies   (WholeType("XYZ"))))
      'impliedBy  - test("-impliedBy:B-12", Not(ImpliedBy (SomeOfType("B", NES(12)))))
      'presence   - test("-has:face",       Not(Presence  ("face")))
      'lack       - test("-no:hair",        Not(Lack      ("hair")))
      'allOf      - test("-(my god)",       Not(allOf     (SimpleText("my"), SimpleText("god"))))
      'anyOf      - test("-{my god}",       Not(anyOf     (SimpleText("my"), SimpleText("god"))))
      'not        - test("--whip",          SimpleText("whip"))
    }

    // TODO {has,no,implies,impliedBy}: etc case insensitive

    // TODO MF FR should warn and recommend {MF FR}
    // Or do it automatically if parsing isn't live

    'combos {
      'anyAll - test("{(MF)}",      anyOf(allOf(ReqType("MF"))))
      'allAny - test("({MF})",      allOf(anyOf(ReqType("MF"))))

      * - test("abc  {MF FR}  -( eat drink {has:a has:b})", allOf(
            SimpleText("abc"),
            anyOf(ReqType("MF"), ReqType("FR")),
            Not(allOf(SimpleText("eat"), SimpleText("drink"), anyOf(Presence("a"), Presence("b"))))
          ))
    }
  }
}
