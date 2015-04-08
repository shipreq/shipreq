package shipreq.webapp.base.text

import java.util.concurrent.atomic.AtomicInteger
import japgolly.nyaya._
import japgolly.nyaya.util._
import japgolly.nyaya.test._
import japgolly.nyaya.test.PropTest._
import org.parboiled2._
import scala.util.{Try, Failure, Success}
import scalaz.Equal
import scalaz.std.list._
import scalaz.std.stream._
import utest._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.{RandomData => $}
import shipreq.webapp.base.test.SampleProject
import shipreq.webapp.base.test.BaseTestUtil._
import Text.AtomType

object ParsersTest extends TestSuite {

  val counts = AtomType.values.toStream.map((_, new AtomicInteger)).toMap
  def count(as: Iterable[Text.Generic#Atom]): Unit =
    as.foreach { a =>
      val t = AtomType of a
      counts(t).incrementAndGet()
    }

  class Tester(p: Project, inputs: List[String]) {
    override def toString = p.toString

    println(p.countAtoms.showTree + "\n")

    val E = EvalOver(this)

    val txt2str = Presentation.textToString(p)

    val genericReqs = p.reqs.data.reqs.values.filterT[GenericReq]

    def cmp[A <: Text.Generic#Atom](t: String, actual: Iterable[A], expect: Iterable[A]): EvalL = {

      var a = actual.toVector
      var e = expect.toVector
      while (a.nonEmpty && e.nonEmpty && a.head == e.head) {
        a = a.tail
        e = e.tail
      }
      while (a.nonEmpty && e.nonEmpty && a.last == e.last) {
        a = a.init
        e = e.init
      }

      E.equal(t, a, e)(Equal.equalA)
    }

    def testGenericReqDesc(r: GenericReq) = {
      val src = r.desc
      count(src)
      val txt = txt2str(src)
      val parsed = new Parsers.GenericReqDescParser(p, txt).main.run().get
      cmp(txt, parsed, src)
    }

    def testString(in0: String) = {
      val in1  = Parsers.preprocess(in0)
      val out1 = new Parsers.GenericReqDescParser(p, in1).main.run().get
      val in2  = txt2str(out1)
      val out2 = new Parsers.GenericReqDescParser(p, in2).main.run().get
      count(out1)
      cmp(in1, out1, out2)
    }

    def all = (
      E.forall(genericReqs)(testGenericReqDesc).rename("toStr |> parse = id")
        ∧ E.forall(inputs)(testString).rename("parse |> toStr |> parse = parse")
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
    (_: T.EmailAddress).value, newGenericReqDescParser)(_.emailAddress.run())

  def propWebAddress = parserProp("WebAddress",
    (_: T.WebAddress).value, newGenericReqDescParser)(_.webAddress.run())

  def propMathTeX = parserProp("MathTeX",
    (_: T.MathTeX).value |> Grammar.mathTexSurround.display, newGenericReqDescParser)(_.mathtex.run())

  // #TODO{ <math>\frac{22}</math> }

  override val tests = TestSuite {
    'manual {
      'hashHashHash {
        () // TODO
      }
    }

    'small {
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
