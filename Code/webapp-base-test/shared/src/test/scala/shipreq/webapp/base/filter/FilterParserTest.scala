package shipreq.webapp.base.filter

import japgolly.microlibs.nonempty._
import nyaya.gen.Gen
import nyaya.prop._
import nyaya.test._
import nyaya.test.PropTestOps._
import org.parboiled2.ErrorFormatter
import utest._
import shipreq.base.util.Debug._
import shipreq.base.util._
import shipreq.webapp.base.{RandomData => $}
import shipreq.webapp.base.data.HashRefKey
import shipreq.webapp.base.data.ReqType.Mnemonic
import shipreq.webapp.base.test.WebappTestUtil._
import PotentialFilter._

object FilterParserTest extends TestSuite {

  implicit def equality: UnivEq[PotentialFilter] = UnivEq.force

  implicit def autoSome(f: PotentialFilter) = Option(f)
  implicit def autoMne(s: String) = Mnemonic(s)
  implicit def autoHRK(s: String) = HashRefKey(s)
  implicit def autoNev[A](a: A) = NonEmptyVector(a)
  implicit def NES[A: UnivEq](a: A, as: A*) = NonEmptySet(a, as.toSet)

  def allOf(a: PotentialFilter, b: PotentialFilter*) = AllOf(NonEmptyVector(a, b: _*))
  def anyOf(a: PotentialFilter, b: PotentialFilter*) = AnyOf(NonEmptyVector(a, b: _*))

  def postProcess(pf: PotentialFilter): PotentialFilter =
    pf match {
      case PotentialFilter.AllOf(as) if as.length == 1 => postProcess(as.head)
      case PotentialFilter.AnyOf(as) if as.length == 1 => postProcess(as.head)
      case _ => pf
    }

  def propFromString = Prop.eval[String] { s1 =>
    val e = EvalOver(s1)
    FilterParser.parse(s1) match {
      case FilterParser.Result.BlankFilter
         | _: FilterParser.Result.ParseException
         | _: FilterParser.Result.GeneralException => e.pass
      case FilterParser.Result.Filter(pf0) =>
        val pf1 = postProcess(pf0)

        // fix before comparison
//        def normaliseFilterText(s: String): String =
//          FilterParser.preProcessor(s).asString
//            .filterNot(java.lang.Character.isWhitespace)
//            .map(c => if (java.lang.Character.isWhitespace(c)) ' ' else c)
//            .replaceAll(" {2,}", " ")
            // .map(c => "%04x".format(c.toInt)).mkString(",")

        val s2 = PotentialFilter.toText(pf1)
        val pf2 = toOption(FilterParser.parse(s2)).map(postProcess)
        e.equal("parse = parse.toText.parse", pf2, Option(pf1))
    }
  }

  def propFromPF = Prop.eval[PotentialFilter] { pf0 =>
    val pf1 = postProcess(pf0)
    val s = PotentialFilter.toText(pf1)
    val e = EvalOver(s)
    val pf2 = toOption(FilterParser.parse(s)).map(postProcess)
    e.equal("parse . toText = id", pf2, Option(pf1))
  }

  val toOption: FilterParser.Result => Option[PotentialFilter] = {
    case FilterParser.Result.Filter(f)           => Some(f)
    case FilterParser.Result.BlankFilter         => None
    case FilterParser.Result.GeneralException(e) => fail("Parser failed: " + Option(e.getMessage).getOrElse(e.toString))
    case e: FilterParser.Result.ParseException   => println(e.format(new ErrorFormatter(showTraces = true))); fail("Parser failed.")
  }

  def test(str: String, exp: Option[PotentialFilter]): Unit =
    assertEq(s"[$str]", toOption(FilterParser.parse(str)), exp)

  def testFail(str: String): Unit =
    FilterParser.parse(str) match {
      case _: FilterParser.Result.ParseException
         | _: FilterParser.Result.GeneralException => ()
      case FilterParser.Result.Filter(f)           => fail(s"[$str] succeeded with $f")
      case FilterParser.Result.BlankFilter         => fail(s"[$str] succeeded with <blank>")
    }

  override def tests = TestSuite {

    'prop {
      import PropTest.defaultPropSettings
//      implicit def settings = DefaultSettings.propSettings.setSampleSize(100000).setDebug
      'fromStringA - propFromString.mustBeSatisfiedBy(Gen.ascii.string1)
      'fromStringU - propFromString.mustBeSatisfiedBy(Gen.unicode.string1)
      'fromPF      - propFromPF    .mustBeSatisfiedBy($.filter.potential.gen)
//      Gen.ascii.string1.bugHunt(74)(propFromString)
//      Gen.unicode.string1.bugHunt()(propParseUnparse)
//      $.filter.potential.gen.bugHunt(samplesPerSeed = 7)(propUnparseParse)
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
