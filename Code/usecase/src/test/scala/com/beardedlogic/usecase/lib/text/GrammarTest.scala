package com.beardedlogic.usecase
package lib.text

import org.scalatest.FunSpec
import org.scalatest.prop.{TableFor2, PropertyChecks}
import test.TestHelpers
import lib.Types._
import ParsingConfig.{FlowToStyle, FlowStyle, FlowFromStyle}
import Grammar._

class GrammarTest extends FunSpec with TestHelpers with PropertyChecks {
  val G = Grammar

  describe("Grammar") {
    def test[T](parser: G.Parser[T], examples: TableFor2[String, Boolean])(expect: String => T) {
      forAll(examples)((input, pass) => {
        val r = G.parseAll(parser, input)
        r.successful should be(pass)
        if (pass) r.get should be(expect(input))
      })
    }

    it("should parse StepLabel") {
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
      test(G.StepLabel, examples)(_.replaceAll("\\s+", ""))
    }

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
      test(G.ValidRef, examples)(i => validRef(i.replaceAll("[\\s\\[\\]]+", "")))
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
      test(G.FlowRefList, examples) {
        _.replace("[", ",[").replace("]", "],").replaceAll("[\\s\\[\\]]+", "").split(",+").filter(_.nonEmpty).toList.map(validRef)
      }
    }

    it("should parse TextAndFlows with flow") {
      forAll(TextWithFlowExamples) {
        (input, expText, expRefsFrom, expRefsTo) =>
          withClue(s"Input='$input'.") {
            val r = G.parseAll(G.TextAndFlows, input)
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

  def validRefS(x: RefToken): String = validRef(x)
  def validRef(x: RefToken): StepLabel = x match {
    case PotentiallyValidRef(lbl) => lbl
    case _ => fail("Expected result: " + x)
  }

  def validRef(x: String) = PotentiallyValidRef(x.asLabel)
}