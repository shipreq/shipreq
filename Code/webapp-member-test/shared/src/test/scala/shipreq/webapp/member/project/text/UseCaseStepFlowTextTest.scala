package shipreq.webapp.member.project.text

import nyaya.gen._
import nyaya.prop._
import nyaya.test.PropTest._
import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.{Backwards, Forwards, Util}
import shipreq.webapp.base.util.CharSubset
import shipreq.webapp.member.project.text.UseCaseStepFlowText.Elem
import utest._

object UseCaseStepFlowTextTest extends TestSuite {

  val genInput: Gen[String] = {
    val arrowStem = Gen.pure('-').list(2 to 8)

    val arrow = for {
      b <- Gen.boolean
      s <- arrowStem
    } yield if (b) '<' :: s else s ::: '>' :: Nil


    val arrowLike = Gen.chooseChar('-', "<>").list(1 to 8)

//    val text = shipreq.webapp.member.test.RandomData.unicodeChar.list(8)
     val text = Gen.chooseChar(' ', '!' to '\u4000').list(8)

    val stepSeparator = Gen.choose(' ', ',').list(1 to 4)

    Gen.chooseGen(arrow, arrowLike, stepSeparator, text, text)
      .list(0 to 8)
      .map(llc => Util.quickSB(sb => llc foreach (_ foreach sb.append)))
  }

  val arrowRegexS =
    CharSubset.PunctuationOrSymbol.regexNotAround("<-{2,}|-{2,}>")

  val arrowRegex =
    arrowRegexS.r.pattern

  val flowRegex =
    (arrowRegexS + "[^\r\n]*").r

  class Tester(input: String) {
    override def toString = input.quote
    val E = EvalOver(this)

    val parsed = UseCaseStepFlowText.parse(input).toList

    val inputWithoutFlow = flowRegex.replaceAllIn(input, "$1")

    val parsedWithoutFlow = UseCaseStepFlowText.parse(inputWithoutFlow).toList

    def elems =
      E.forall(parsed) {
        case Elem.Text(t)  => E.test("Text is non-empty.", t.nonEmpty) &
                              E.test("No arrows in text.", !arrowRegex.matcher(t).matches)
        case Elem.Step(s)  => E.test("Step is non-empty.", s.nonEmpty) &
                              E.test("No whitespace in step.", !s.exists(Character.isWhitespace))
        case Elem.Arrow(_) => E.pass
      }

    def noConsecutiveText =
      E.atom("No consecutive text",
        (parsed.foldLeft[Boolean \/ String](-\/(false)) {
          case (-\/(true ), Elem.Text(t)) => \/-(s"Found next to text: '$t'")
          case (-\/(false), Elem.Text(_)) => -\/(true)
          case (-\/(_)    , _           ) => -\/(false)
          case (r@ \/-(_) , _           ) => r
        }).toOption)

    def nonFirstText =
      E.forall(parsed drop 1) {
        case Elem.Text(t) => E.test("Non-first text must start with NL.", t.headOption.exists(c => c ==* '\n' || c ==* '\r'))
        case _            => E.pass
      }

    def sole =
      parsed match {
        case Elem.Text(t) :: Nil => E.equal("Sole text = input", t, input)
        case _                   => E.pass
      }

    def withoutFlow =
      E.equal("Input without flow", parsedWithoutFlow,
        if (inputWithoutFlow.isEmpty) Nil else Elem.Text(inputWithoutFlow) :: Nil)

    val all = (elems & noConsecutiveText & nonFirstText & sole & withoutFlow) rename "all"
  }

  val genTester: Gen[Tester] =
    genInput.map(new Tester(_))

  override def tests = Tests {

    "spotCheck" - {
      import Elem._
      val input = " let's try -->1<-- 2 -->3  4. <--- --> 5,6 ,, 7"
      val parsed = UseCaseStepFlowText.parse(input).toList
      assertEq(parsed, expect = List(
        Text(" let's try "),
        Arrow(Forwards), Step("1"),
        Arrow(Backwards), Step("2"),
        Arrow(Forwards), Step("3"), Step("4."),
        Arrow(Backwards), Arrow(Forwards), Step("5"), Step("6"), Step("7")
      ))
    }

    "prop" - {
      // genTester.bugHunt(0, 10000)(Prop.eval(_.all))
      genTester.mustSatisfyE(_.all) //(defaultPropSettings.setDebug)
    }
  }
}
