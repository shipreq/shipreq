package com.beardedlogic.usecase
package feature.uc.text

import org.scalatest.FunSpec
import org.scalatest.prop.{TableFor2, PropertyChecks}
import test.TestHelpers
import lib.Types._
import ParsingConfig.{FlowToStyle, FlowStyle, FlowFromStyle}
import Grammar._
import feature.uc.text.Grammar.FreeTextToken.MathTexToken

class GrammarTest extends FunSpec with TestHelpers with PropertyChecks {
  val G = Grammar

  def test[T](parser: G.Parser[T], examples: TableFor2[String, Boolean])(expect: String => T): Unit = {
    forAll(examples)((input, pass) => {
      val r = G.parseAll(parser, input)
      r.successful should be(pass)
      if (pass) r.get should be(expect(input))
    })
  }

  def test2[T](parser: G.Parser[T], examples: TableFor2[String, Option[T]]): Unit = {
    forAll(examples)((input, expOp) => {
      val r = G.parseAll(parser, input)
      expOp match {
        case None =>
          r.successful shouldBe false
        case Some(exp) =>
          r.successful shouldBe true
          r.get shouldBe exp
      }
    })
  }

  it("should parse StepLabels") {
    val examples = Table(("EXAMPLE", "PASS")
      , ("1.0", true)
      , ("1.0.a", true)
      , ("1.0.a.iii", true)
      , ("1.0.a.iii.1", true)
      , ("1.E.2", true)
      , ("13.E.20.ba.xiv.23", true)
      , ("13. E . 20  .ba.xiv.23", true)
      , ("1.", false)
      , (".0", false)
      , ("1", false)
      , ("1..0", false)
      , ("X1", false)
      , ("", false)
    )
    test(G.FreeTextParsers.StepLabel, examples)(_.replaceAll("\\s+", ""))
  }

  it("should parse math.tex") {
    implicit def autoResult(s: String) = Some(MathTexToken(s))
    val e = """\left( \sum_{k=1}^n a_k b_k \right)^2 \leq \left( \sum_{k=1}^n a_k^2 \right) \left( \sum_{k=1}^n b_k^2 \right)"""
    val examples = Table[String, Option[MathTexToken]](("IN", "OUT")
      , ("{|math.tex: 1+1|}", "1+1")
      , ("{|MATH.TEX: 1+3|}", "1+3")
      , ("{|  math.tex  :   1+2  |}", "1+2")
      , (s"{|math.tex: $e |}", e)
      , ("{|math.tex: 1+1|", None)
      , ("{|math.tex: 1+1 }", None)
      , ("{|math.tex:    |}", None)
    )
    test2(G.FreeTextParsers.MathTex, examples)
  }

  describe("FlowParsers") {
    import FlowParsers._

    def validRefS(x: RefToken): String = validRefL(x)

    def validRefL(x: RefToken): StepLabel = x match {
      case PotentiallyValidRef(lbl) => lbl
      case _ => fail("Expected result: " + x)
    }

    def validRef(x: String) = PotentiallyValidRef(x.asLabel)

    it("should parse ValidRef") {
      val examples = Table(("EXAMPLE", "PASS")
        , ("1.0", true)
        , ("1.0.a.iii.1", true)
        , ("1.E.2", true)
        , ("13.E.20.ab.xiv.23", true)
        , ("[1.0]", true)
        , ("[13.E.20.ab.xiv.23]", true)
        , ("[ 1.0 ]", true)
        , ("[ 13.E.20.ab.xiv.23 ]", true)
        , ("[1.0", false)
        , ("1.0]", false)
        , ("[[1.0]", false)
        , ("[1.0]]", false)
        , ("[[1.0]]", false)
        , ("", false)
      )
      test(ValidRef, examples)(i => validRef(i.replaceAll("[\\s\\[\\]]+", "")))
    }

    it("should parse FlowRefList") {
      val examples = Table(("EXAMPLE", "PASS")
        , ("1.0", true)
        , ("1.0, 1.2", true)
        , ("[1.0], [1.2]", true)
        , ("[1.0] [1.2]", true)
        , ("[1.0] 1.2", true)
        , ("1.3 [1.0]", true)
        , ("1.0, 1.2. a, 1.3, 1.1", true)
        , ("[1.0] 1.2. a, [1.3] [1.1]", true)
        , ("", false)

      )
      test(FlowRefList, examples) {
        _.replace("[", ",[").replace("]", "],").replaceAll("[\\s\\[\\]]+", "").split(",+").filter(_.nonEmpty).toList.map(validRef)
      }
    }

    it("should parse TextAndFlows with flow") {
      forAll(TextWithFlowExamples) {
        (input, expText, expRefsFrom, expRefsTo) =>
          withClue(s"Input='$input'.") {
            val r = G.parseAll(TextAndFlowClauses, input)
            withClue("Parse result should be successful.") {r.successful ==== true}
            r.get._1 should be(expText)
            val flowResults = r.get._2
            def results(style: FlowStyle) = flowResults.filter(_.style == style).map(_.refs).flatten.map(validRefS)
            results(FlowFromStyle) ==== expRefsFrom
            results(FlowToStyle) ==== expRefsTo
          }
      }
    }
  }
}