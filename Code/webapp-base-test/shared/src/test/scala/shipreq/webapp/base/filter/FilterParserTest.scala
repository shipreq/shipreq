package shipreq.webapp.base.filter

import japgolly.microlibs.nonempty._
import nyaya.gen.Gen
import nyaya.prop._
import nyaya.test._
import nyaya.test.PropTestOps._
import org.parboiled2.ErrorFormatter
import scalaz.{-\/, Functor, \/-}
import utest._
import shipreq.base.util.Debug._
import shipreq.base.util._
import shipreq.webapp.base.{RandomData => $}
import shipreq.webapp.base.data.{ExternalPubid, HashRefKey, ReqTypePos}
import shipreq.webapp.base.data.ReqType.Mnemonic
import shipreq.webapp.base.filter._
import shipreq.webapp.base.filter.Filter.{Potential, PotentialF}
import shipreq.webapp.base.filter.Filter.Implicits._
import shipreq.webapp.base.filter.IntensionalReqSet._
import shipreq.webapp.base.test.WebappTestUtil._

object FilterParserTest extends TestSuite {

  import Filter.Potential._

  implicit def autoSome(f: Potential) = Option(f)
  implicit def autoMne(s: String) = Mnemonic(s)
  implicit def autoHRK(s: String) = HashRefKey(s)
  implicit def autoRS(s: IntensionalReqSet[String]): Potential.ReqSet = NonEmptyVector one Functor[IntensionalReqSet].map(s)(autoMne)
  implicit def autoNev[A](a: A) = NonEmptyVector(a)
  implicit def NES[A: UnivEq](a: A, as: A*) = NonEmptySet(a, as.toSet)

  def postProcess(pf: Potential): Potential =
    pf.unfix match {
      case FilterAst.AllOf(as) if as.length == 1 => postProcess(as.head)
      case FilterAst.AnyOf(as) if as.length == 1 => postProcess(as.head)
      case _ => pf
    }

  def propFromString = Prop.eval[String] { s1 =>
    val e = EvalOver(s1)
    FilterParser.parse(s1) match {
      case \/-(None)
         | -\/(_: FilterParser.ParseException)
         | -\/(_: FilterParser.GeneralException) => e.pass

      case \/-(Some(pf0)) =>
        val pf1 = postProcess(pf0)

        // fix before comparison
//        def normaliseFilterText(s: String): String =
//          FilterParser.preProcessor(s).asString
//            .filterNot(java.lang.Character.isWhitespace)
//            .map(c => if (java.lang.Character.isWhitespace(c)) ' ' else c)
//            .replaceAll(" {2,}", " ")
            // .map(c => "%04x".format(c.toInt)).mkString(",")

        val s2 = Potential.toText(pf1)
        val pf2 = toOption(FilterParser.parse(s2)).map(postProcess)
        e.equal("parse = parse.toText.parse", pf2, Option(pf1))
    }
  }

  def propFromPF = Prop.eval[Potential] { pf0 =>
    val pf1 = postProcess(pf0)
    val s = Potential.toText(pf1)
    val e = EvalOver(s)
    val pf2 = toOption(FilterParser.parse(s)).map(postProcess)
    e.equal("parse . toText = id", pf2, Option(pf1))
  }

  val toOption: FilterParser.Result => Option[Potential] = {
    case \/-(f)                                => f
    case -\/(FilterParser.GeneralException(e)) => fail("Parser failed: " + Option(e.getMessage).getOrElse(e.toString))
    case -\/(e: FilterParser.ParseException)   => println(e.format(new ErrorFormatter(showTraces = true))); fail("Parser failed.")
  }

  def test(str: String, exp: Option[Potential]): Unit =
    assertEq(s"[$str]", toOption(FilterParser.parse(str)), exp)

  def testFail(str: String): Unit =
    FilterParser.parse(str) match {
      case -\/(_)       => ()
      case \/-(Some(f)) => fail(s"[$str] succeeded with $f")
      case \/-(None)    => fail(s"[$str] succeeded with <blank>")
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
      def testSimple(str: String): Unit = test(str, text(str))
      'simple      - testSimple("a")
      'blurt       - testSimple("weqfd351!")
      'singleQuote - testSimple("they're")
      'longUpper   - testSimple("Q"*30)
      'underscore  - testSimple("L5_X")
    }

    'quotedText {
      'empty  - testFail("''")
      'open1  - testFail("'")
      'open2  - testFail("'asdf")
      'open3  - testFail("'asdf`")
      'simple - test("'hehe'",        text("hehe", '\''))
      'qmix1  - test("""'1 ` " 2'""", text("""1 ` " 2""", '\''))
      'qmix2  - test("""`1 ' " 2`""", text("""1 ' " 2""", '`'))
      'qmix3  - test(""""1 ' ` 2"""", text("""1 ' ` 2""", '"'))
    }

    'regex {
      'empty  - testFail("//")
      'open1  - testFail("/")
      'open2  - testFail("/asdf asdf ")
      'simple - test("/a/", regex("a"))
      'qchars - test("/'.*`.*`'/", regex("'.*`.*`'"))
      'escape - test("""/\\ \. \d \s \n \/ \W/""", regex("""\\ \. \d \s \n / \W"""))
    }

    'reqs {
      'single       - test("MF-3",         reqs(SomeOfType("MF", NES(3))))
      'singleNoDash - test("MF3",          reqs(SomeOfType("MF", NES(3))))
      'range        - test("MF-{2,4,6-8}", reqs(SomeOfType("MF", NES(2,4,6,7,8))))
      'rangeNoDash  - test("MF{2,4,6-8}",  reqs(SomeOfType("MF", NES(2,4,6,7,8))))
      'lowerCase    - test("mf-3",         text("mf-3"))
      'quoted       - test("'MF-3'",       text("MF-3", '\''))
    }

    'reqType {
      * - test("X",  reqType("X"))
      * - test("MF", reqType("MF"))
      * - test("BOBOP", reqType("BOBOP"))
    }

    'hashRef {
      'empty1  - testFail("#")
      'empty2  - testFail("# ")
      'simple1 - test("#x",       hashRef("x"))
      'simple2 - test("#pri=123", hashRef("pri=123"))
      'anyCase - test("#TO"+"DO", hashRef("TO"+"DO"))
    }

    'implication {
      'empty1       - testFail("implies:")
      'empty2       - testFail("impliedBy:  ")
      'open1        - testFail("impliedBy:MF-")
      'open2        - testFail("impliedBy:MF-{")
      'open3        - testFail("impliedBy:MF-{3,4")
      'wholeType    - test("impliedBy:MF",                  impliedByAnyOf(WholeType("MF")))
      'specific     - test("impliedBy:CO-{3,5,7}",          impliedByAnyOf(SomeOfType("CO", NES(3,5,7))))
      'rangeRev     - test("impliedBy:CO-{9-7}",            impliedByAnyOf(SomeOfType("CO", NES(7,8,9))))
      'lowercase    - test("impliedBy:dgh",                 impliedByAnyOf(WholeType("DGH")))
      'single       - test("implies:MF-2",                  impliesAnyOf  (SomeOfType("MF", NES(2))))
      'singleNoDash - test("implies:MF2",                   impliesAnyOf  (SomeOfType("MF", NES(2))))
      'range        - test("implies:CO-{3-6}",              impliesAnyOf  (SomeOfType("CO", NES(3,4,5,6))))
      'rangeNoDash  - test("implies:CO{3-6}",               impliesAnyOf  (SomeOfType("CO", NES(3,4,5,6))))
      'combo        - test("implies:SI,DD-{3-5,9,12-14,1}", impliesAnyOf  (reqSet(WholeType("SI"), SomeOfType("DD", NES(3,4,5,9,12,13,14,1)))))
    }

    'presence {
      'empty  - testFail("has:")
      'simple - test("has:stuff", presence("stuff"))
    }

    'lack {
      'empty  - testFail("no:")
      'simple - test("no:stuff", lack("stuff"))
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
      'one    - test("(MF)",        allOf(reqType("MF")))
      'oneWS  - test("(   MF  )",   allOf(reqType("MF")))
      'two    - test("(MF BR)",     allOf(reqType("MF"), reqType("BR")))
      'three  - test("(#X #Y yay)", allOf(hashRef("X"), hashRef("Y"), text("yay")))
      'nest   - test("((MF))",      allOf(allOf(reqType("MF"))))
    }

    'anyOf {
      'empty1 - testFail("{}")
      'empty2 - testFail("{  }")
      'open1  - testFail("{")
      'open2  - testFail("{MF")
      'one    - test("{MF}",        anyOf(reqType("MF")))
      'oneWS  - test("{   MF  }",   anyOf(reqType("MF")))
      'two    - test("{MF BR}",     anyOf(reqType("MF"), reqType("BR")))
      'three  - test("{#X #Y yay}", anyOf(hashRef("X"), hashRef("Y"), text("yay")))
      'nest   - test("{{MF}}",      anyOf(anyOf(reqType("MF"))))
    }

    'not {
      'empty1     - testFail("-")
      'empty2     - testFail("--")
      'simpleText - test("-car",            not(text          ("car")))
      'quotedText - test("-'eat food'",     not(text          ("eat food", '\'')))
      'regex      - test("-/a .*b/",        not(regex         ("a .*b")))
      'reqType    - test("-MF",             not(reqType       ("MF")))
      'hashRef    - test("-#boo",           not(hashRef       ("boo")))
      'implies    - test("-implies:XYZ",    not(impliesAnyOf  (WholeType("XYZ"))))
      'impliedBy  - test("-impliedBy:B-12", not(impliedByAnyOf(SomeOfType("B", NES(12)))))
      'presence   - test("-has:face",       not(presence      ("face")))
      'lack       - test("-no:hair",        not(lack          ("hair")))
      'allOf      - test("-(my god)",       not(allOf         (text("my"), text("god"))))
      'anyOf      - test("-{my god}",       not(anyOf         (text("my"), text("god"))))
      'not        - test("--whip",          text("whip"))
    }

    // TODO {has,no,implies,impliedBy}: etc case insensitive

    // TODO MF FR should warn and recommend {MF FR}
    // Or do it automatically if parsing isn't live

    'combos {
      'anyAll - test("{(MF)}",      anyOf(allOf(reqType("MF"))))
      'allAny - test("({MF})",      allOf(anyOf(reqType("MF"))))

      * - test(" a b_c ", allOf(text("a"), text("b_c")))

      * - test("abc  {MF FR}  -( eat drink {has:a has:b})", allOf(
            text("abc"),
            anyOf(reqType("MF"), reqType("FR")),
            not(allOf(text("eat"), text("drink"), anyOf(presence("a"), presence("b"))))
          ))
    }
  }
}
