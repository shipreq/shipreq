package shipreq.webapp.base.text

import java.util.concurrent.atomic.AtomicInteger
import japgolly.nyaya._
import japgolly.nyaya.util._
import japgolly.nyaya.test._
import japgolly.nyaya.test.PropTest._
import org.parboiled2._
import scala.util.{Try, Failure, Success}
import scalaz.{NonEmptyList, Equal}
import scalaz.std.list._
import scalaz.std.stream._
import utest._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.{RandomData => $}
import shipreq.webapp.base.test.SampleProject
import shipreq.webapp.base.test.BaseTestUtil._

object ParsersTest extends TestSuite {

  sealed trait AtomType
  object AtomType {
    case object Literal       extends AtomType
    case object NewLine       extends AtomType
    case object ReqRef        extends AtomType
    case object Issue         extends AtomType
    case object WebAddress    extends AtomType
    case object EmailAddress  extends AtomType
    case object MathTeX       extends AtomType
    case object TagRef        extends AtomType
    case object UnorderedList extends AtomType

    val values = NonEmptyList[AtomType](
      Literal, NewLine, ReqRef, Issue, WebAddress, EmailAddress, MathTeX, TagRef, UnorderedList)

    val of: Text.Generic#Atom => AtomType = {
      case _: Text.Generic.Literal         # Literal       => Literal
      case _: Text.Generic.NewLine         # NewLine       => NewLine
      case _: Text.Generic.ReqRef          # ReqRef        => ReqRef
      case _: Text.Generic.Issue           # Issue         => Issue
      case _: Text.Generic.PlainTextMarkup # WebAddress    => WebAddress
      case _: Text.Generic.PlainTextMarkup # EmailAddress  => EmailAddress
      case _: Text.Generic.PlainTextMarkup # MathTeX       => MathTeX
      case _: Text.Generic.TagRef          # TagRef        => TagRef
      case _: Text.Generic.ListMarkup      # UnorderedList => UnorderedList
    }
  }

  val counts = AtomType.values.list.map((_, new AtomicInteger)).toMap

  def count(as: List[Text.Generic#Atom]): Unit =
    as.foreach { a =>
      val t = AtomType of a
      counts(t).incrementAndGet()
    }

  class Tester(p: Project, inputs: List[String]) {
    override def toString = "TODO" // TODO add project size info

    val E = EvalOver(this)

    val txt2str = Presentation.textToString(p)

    val genericReqs = p.reqs.data.reqs.values.filterT[GenericReq]

    def testGenericReqDesc(r: GenericReq) = {
      val src = r.desc
      count(src)
      val txt = txt2str(src)
      val parsed = new Parsers.GenericReqDescParser(p, txt).main.run().get
      E.equal(txt, parsed, src)(Equal.equalA)
    }

    def testString(in0: String) = {
      val in1  = Parsers.preprocess(in0)
      val out1 = new Parsers.GenericReqDescParser(p, in1).main.run().get
      val in2  = txt2str(out1)
      val out2 = new Parsers.GenericReqDescParser(p, in2).main.run().get
      count(out1)
      E.equal(in1, out1, out2)(Equal.equalA)
    }

    def all = (
      E.forall(genericReqs)(testGenericReqDesc).rename("txt2str |> parse = id")
        ∧ E.forall(inputs)(testString).rename("parse |> txt2str |> parse = parse")
      )
  }

  def tester: Gen[Tester] =
    for {p <- $.project; ss <- Gen.string1.list1.map(_.list)} yield new Tester(p, ss)

  // -------------------------------------------------------------------------------------------------------------------

  def parserProp[A, P <: Parser](name: => String, txt: A => String, parser: ParserInput => P)(run: P => Try[A])(implicit eq: Equal[A]) =
    Prop.atom[A](name,
      a => {
        val p = parser(txt(a))
        val r = run(p)
        r match {
          case Success(b) =>
            if (eq.equal(a, b)) None else Some(s"Expected $a, got $b")
          case Failure(e: ParseError) =>
            Some(p.formatError(e, new ErrorFormatter(showTraces = true)))
          case Failure(e) =>
            Some(e.toString)
        }
      }
    )

  val T = Text.GenericReqDesc
  lazy val newGenericReqDescParser = new Parsers.GenericReqDescParser(SampleProject.project, _: ParserInput)

  def propEmailAddress = parserProp("EmailAddress",
    (_: T.EmailAddress).value, newGenericReqDescParser)(_.emailAddress.run())(Equal.equalA)

  def propWebAddress = parserProp("WebAddress",
    (_: T.WebAddress).value, newGenericReqDescParser)(_.webAddress.run())(Equal.equalA)

  def propMathTeX = parserProp("MathTeX",
    (_: T.MathTeX).value |> Grammar.mathTexSurround.display, newGenericReqDescParser)(_.mathtex.run())(Equal.equalA)

  override val tests = TestSuite {
    'unit {
      'emailAddress - $.TextGen.emailAddress(T).mustSatisfy(propEmailAddress)
      'webAddress   - $.TextGen.webAddress  (T).mustSatisfy(propWebAddress)
      'mathtex      - $.TextGen.mathTex     (T).mustSatisfy(propMathTeX)
    }

    'big {
      tester.mustSatisfyE(_.all) //(DefaultSettings.propSettings.setDebug.setSampleSize(50).setSeed(0).setGenSize(50))
      println()
      val graphUnit = 1000 `JVM|JS` 10
      val graphChar = "#" `JVM|JS` "."
      println("Parser test distribution")
      println("========================")
      AtomType.values.foreach { t =>
        val c = counts(t).get()
        printf("%-13s :%6d | %s\n", t.toString, c, graphChar * (c / graphUnit))
      }
      println()
    }
  }
}
