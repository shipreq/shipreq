package shipreq.webapp.base.filter

import japgolly.microlibs.nonempty._
import nyaya.gen.Gen
import nyaya.prop._
import nyaya.test.PropTestOps._
import nyaya.test._
import org.parboiled2.ErrorFormatter
import scalaz.{-\/, Functor, \/-}
import shipreq.webapp.base.data.ReqType.Mnemonic
import shipreq.webapp.base.data.{HashRefKey, Off, On, ProjectConfig}
import shipreq.webapp.base.filter.Filter.Implicits._
import shipreq.webapp.base.filter.Filter.{Potential, Valid}
import shipreq.webapp.base.filter.IntensionalReqSet._
import shipreq.webapp.base.filter._
import shipreq.webapp.base.test.WebappTestUtil._
import shipreq.webapp.base.{RandomData => $}
import sourcecode.Line
import utest._

object FilterParserTest extends TestSuite {

  import Filter.Potential._

  private implicit def autoSome(f: Potential) = Option(f)
  private implicit def autoMne(s: String) = Mnemonic(s)
  private implicit def autoHRK(s: String) = HashRefKey(s)
  private implicit def autoRS(s: IntensionalReqSet[String]): Potential.ReqSet = NonEmptyVector one Functor[IntensionalReqSet].map(s)(autoMne)
  private implicit def NES[A: UnivEq](a: A, as: A*) = NonEmptySet(a, as.toSet)

  private def postProcess(pf: Potential): Potential =
    pf.unfix match {
      case FilterAst.AllOf(as) if as.length == 1 => postProcess(as.head)
      case _ => pf
    }

  private def propFromString = Prop.eval[String] { s1 =>
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

  private def propFromPF = Prop.eval[Potential] { pf0 =>
    val pf1 = postProcess(pf0)
    val s = Potential.toText(pf1)
    val e = EvalOver(s)
    val pf2 = toOption(FilterParser.parse(s)).map(postProcess)
    e.equal("parse . toText = id", pf2, Option(pf1))
  }

  private def propFromValid(cfg: ProjectConfig) = Prop.eval[Valid] { f =>
    val txt = Filter.Valid.toText(cfg, f)
    val e = EvalOver(f)
    val v = FilterAlgebra.validate(cfg)
    val r = FilterParser.parse(txt).leftMap(_.toString).map(_.map(Filter.Potential.validate(_, v)))
    e.equal("toText |> parse = id", r, \/-(Some(\/-(f))))
  }

  private val toOption: FilterParser.Result => Option[Potential] = {
    case \/-(f)                                => f
    case -\/(FilterParser.GeneralException(e)) => fail("Parser failed: " + Option(e.getMessage).getOrElse(e.toString))
    case -\/(e: FilterParser.ParseException)   => println(e.format(new ErrorFormatter(showTraces = true))); fail("Parser failed.")
  }

  private def test(str: String, exp: Option[Potential])(implicit l: Line): Unit =
    assertEq(s"[$str]", toOption(FilterParser.parse(str)), exp)

  private def testFail(str: String): Unit =
    FilterParser.parse(str) match {
      case -\/(_)       => ()
      case \/-(Some(f)) => fail(s"[$str] succeeded with $f")
      case \/-(None)    => fail(s"[$str] succeeded with <blank>")
    }

  override def tests = Tests {

    "prop" - {
      import PropTest.defaultPropSettings
//      implicit def settings = DefaultSettings.propSettings.setSampleSize(100000).setDebug
      "fromStringA" - propFromString.mustBeSatisfiedBy(Gen.ascii.string1)
      "fromStringU" - propFromString.mustBeSatisfiedBy(Gen.unicode.string1)
      "fromPF"      - propFromPF    .mustBeSatisfiedBy($.filter.potential.gen)
//      Gen.ascii.string1.bugHunt(74)(propFromString)
//      Gen.unicode.string1.bugHunt()(propParseUnparse)
//      $.filter.potential.gen.bugHunt(samplesPerSeed = 7)(propUnparseParse)

      "fromValid" - {
        for (cfg <- $.projectConfig.samples().take(2)) {
          val gen  = $.filter.valid.forProjectConfig(cfg)
          val prop = propFromValid(cfg)
          prop.mustBeSatisfiedBy(gen)
          // gen.bugHunt()(prop)
        }
      }
    }

    "empty" - {
      test("", None)
      test("   ", None)
    }

    "simpleText" - {
      def testSimple(str: String): Unit = test(str, text(str))
      "simple"      - testSimple("a")
      "blurt"       - testSimple("weqfd351!")
      "singleQuote" - testSimple("they're")
      "longUpper"   - testSimple("Q"*30)
      "underscore"  - testSimple("L5_X")
    }

    "quotedText" - {
      "empty"  - testFail("''")
      "open1"  - testFail("'")
      "open2"  - testFail("'asdf")
      "open3"  - testFail("'asdf`")
      "simple" - test("'hehe'",        text("hehe", '\''))
      "qmix1"  - test("""'1 ` " 2'""", text("""1 ` " 2""", '\''))
      "qmix2"  - test("""`1 ' " 2`""", text("""1 ' " 2""", '`'))
      "qmix3"  - test(""""1 ' ` 2"""", text("""1 ' ` 2""", '"'))
    }

    "regex" - {
      "empty"  - testFail("//")
      "open1"  - testFail("/")
      "open2"  - testFail("/asdf asdf ")
      "simple" - test("/a/", regex("a"))
      "qchars" - test("/'.*`.*`'/", regex("'.*`.*`'"))
      "escape" - test("""/\\ \. \d \s \n \/ \W/""", regex("""\\ \. \d \s \n / \W"""))
    }

    "reqs" - {
      "single"       - test("MF-3",         reqs(SomeOfType("MF", NES(3))))
      "singleNoDash" - test("MF3",          reqs(SomeOfType("MF", NES(3))))
      "range"        - test("MF-{2,4,6-8}", reqs(SomeOfType("MF", NES(2,4,6,7,8))))
      "rangeNoDash"  - test("MF{2,4,6-8}",  reqs(SomeOfType("MF", NES(2,4,6,7,8))))
      "lowerCase"    - test("mf-3",         text("mf-3"))
      "quoted"       - test("'MF-3'",       text("MF-3", '\''))
    }

    "reqType" - {
      "1" - test("X",  reqType("X"))
      "2" - test("MF", reqType("MF"))
      "3" - test("BOBOP", reqType("BOBOP"))
    }

    "hashRef" - {
      "empty1"  - testFail("#")
      "empty2"  - testFail("# ")
      "simple1" - test("#x",       hashRef("x"))
      "simple2" - test("#pri=123", hashRef("pri=123"))
      "anyCase" - test("#TO"+"DO", hashRef("TO"+"DO"))
    }

    "implication" - {
      "empty1"       - testFail("implies:")
      "empty2"       - testFail("impliedBy:  ")
      "open1"        - testFail("impliedBy:MF-")
      "open2"        - testFail("impliedBy:MF-{")
      "open3"        - testFail("impliedBy:MF-{3,4")
      "wholeType"    - test("impliedBy:MF",                  impliedByAnyOf(WholeType("MF")))
      "specific"     - test("impliedBy:CO-{3,5,7}",          impliedByAnyOf(SomeOfType("CO", NES(3,5,7))))
      "rangeRev"     - test("impliedBy:CO-{9-7}",            impliedByAnyOf(SomeOfType("CO", NES(7,8,9))))
      "lowercase"    - test("impliedBy:dgh",                 impliedByAnyOf(WholeType("DGH")))
      "single"       - test("implies:MF-2",                  impliesAnyOf  (SomeOfType("MF", NES(2))))
      "singleNoDash" - test("implies:MF2",                   impliesAnyOf  (SomeOfType("MF", NES(2))))
      "range"        - test("implies:CO-{3-6}",              impliesAnyOf  (SomeOfType("CO", NES(3,4,5,6))))
      "rangeNoDash"  - test("implies:CO{3-6}",               impliesAnyOf  (SomeOfType("CO", NES(3,4,5,6))))
      "combo"        - test("implies:SI,DD-{3-5,9,12-14,1}", impliesAnyOf  (reqSet(WholeType("SI"), SomeOfType("DD", NES(3,4,5,9,12,13,14,1)))))
    }

    "presence" - {
      "empty"  - testFail("has:")
      "simple" - test("has:stuff", presence("stuff"))
    }

    "field" - {
      "na"         - test("field:poop=n/a",     fieldProp("poop", "n/a"))
      "default"    - test("field:poop=default", fieldProp("poop", "default"))
      "blank"      - test("field:poop=blank",   fieldProp("poop", "blank"))
      "quoteSpace" - test("field:\"a b\"=xx",   fieldProp("a b", "xx"))
      "quoteColon" - test("field:\"a:b\"=xx",   fieldProp("a:b", "xx"))
      "quoteEqual" - test("field:\"a=b\"=xx",   fieldProp("a=b", "xx"))
      "pos1"       - test("field:X=1",          fieldProp("X", "1"))
      "posM"       - test("field:n=1,3,5-9,40", fieldProp("n", "1,3,5-9,40"))
    }

    "hasIssue" - {
      "on1"  - test("has:issue:x"       , hasIssue(On, "x"))
      "on2"  - test("has:issue:abC,DeF" , hasIssue(On, "abC", "DeF"))
      "off1" - test("has:issue:-x"      , hasIssue(Off, "x"))
      "off2" - test("has:issue:-abC,DeF", hasIssue(Off, "abC", "DeF"))
      "on0"  - testFail("has:issue:")
      "off0" - testFail("has:issue:-")
      "onC"  - testFail("has:issue:x,")
      "offC" - testFail("has:issue:-x,")
    }

    "unknownKeys" - {
      "empty"        - testFail(":")
      "typoLack"     - testFail("noo:stuff")
      "typoPresence" - testFail("hass:stuff")
      "typoImplies"  - testFail("iplies:MF")
      "crap"         - testFail("crap:")
    }

    "allOf" - {
      "empty1" - testFail("()")
      "empty2" - testFail("(  )")
      "open1"  - testFail("(")
      "open2"  - testFail("(MF")
      "one"    - test("(MF)",        allOf(reqType("MF")))
      "oneWS"  - test("(   MF  )",   allOf(reqType("MF")))
      "two"    - test("(MF BR)",     allOf(reqType("MF"), reqType("BR")))
      "three"  - test("(#X #Y yay)", allOf(hashRef("X"), hashRef("Y"), text("yay")))
      "nest"   - test("((MF))",      allOf(allOf(reqType("MF"))))
    }

    "anyOf" - {
      "empty1" - testFail("|")
      "empty2" - testFail("   |    ")
      "missR"  - testFail("MF |")
      "missL"  - testFail("| MF")
      "double" - testFail("MF || BR")
      "double" - testFail("MF | | BR")
      "two"    - test("MF | BR",       anyOf(reqType("MF"), reqType("BR")))
      "three"  - test("#X | #Y | yay", anyOf(hashRef("X"), hashRef("Y"), text("yay")))
      "tight"  - test("#X|#Y|MF|nice", anyOf(hashRef("X"), hashRef("Y"), reqType("MF"), text("nice")))
    }

    "not" - {
      "empty1"     - testFail("-")
      "empty2"     - testFail("--")
      "simpleText" - test("-car",            not(text          ("car")))
      "quotedText" - test("-'eat food'",     not(text          ("eat food", '\'')))
      "regex"      - test("-/a .*b/",        not(regex         ("a .*b")))
      "reqType"    - test("-MF",             not(reqType       ("MF")))
      "hashRef"    - test("-#boo",           not(hashRef       ("boo")))
      "implies"    - test("-implies:XYZ",    not(impliesAnyOf  (WholeType("XYZ"))))
      "impliedBy"  - test("-impliedBy:B-12", not(impliedByAnyOf(SomeOfType("B", NES(12)))))
      "presence"   - test("-has:face",       not(presence      ("face")))
      "allOf"      - test("-(my god)",       not(allOf         (text("my"), text("god"))))
      "anyOf"      - test("-(my|god)",       not(anyOf         (text("my"), text("god"))))
      "not"        - test("--whip",          text("whip"))
    }

    // TODO {has,no,implies,impliedBy}: etc case insensitive

    // TODO MF FR should warn and recommend (MF | FR)
    // Or do it automatically if parsing isn't live

    "combos" - {
      "spaces" - test("a b | c d | 'e|f'", anyOf(allOf(text("a"), text("b")), allOf(text("c"), text("d")), text("e|f", '\'')))

      "parens1" - test("(a b) | (c d)", anyOf(allOf(text("a"), text("b")), allOf(text("c"), text("d"))))

      "parens2" - test("a (b | c) d", allOf(text("a"), anyOf(text("b"), text("c")), text("d")))

      "*" - test(" a b_c ", allOf(text("a"), text("b_c")))

      "*" - test("abc  (MF|FR)  -( eat drink (has:a|has:b))", allOf(
            text("abc"),
            anyOf(reqType("MF"), reqType("FR")),
            not(allOf(text("eat"), text("drink"), anyOf(presence("a"), presence("b"))))
          ))
    }
  }
}
