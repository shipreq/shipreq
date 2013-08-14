package com.beardedlogic.usecase
package test

import net.liftweb.common.Logger
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Prop, Arbitrary, Gen}
import scala.util.matching.Regex

import lib.Misc.removeAllWhitespace
import lib.field._
import lib.tree._
import lib.{Types}
import util.NoReaction

import lib.StepLabels._
import lib.text.ParsingConfig._
import TreeOps._
import Types._
import com.beardedlogic.usecase.lib.field.StepField.StartingLabelIndices

object DataGenerators extends Logger {

  implicit class RegexExt(val x: Regex) extends AnyVal {
    def matches(str: String) = x.pattern.matcher(str).matches()
  }

  private val LabelMakerList = LabelMakers.toList

  implicit class BooleanExt(val x: Boolean) extends AnyVal {
    def :||(err: => String): Prop = Prop(x) :| (if (x) "" else err)
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Low level

  implicit lazy val arbChar: Arbitrary[Char] = Arbitrary {
    val minChar: Char = 32
    Gen.frequency(
      (0xD800 - minChar, Gen.choose(minChar, (0xD800 - 1).asInstanceOf[Char])),
      (Char.MaxValue - 0xDFFF, Gen.choose((0xDFFF + 1).asInstanceOf[Char], Char.MaxValue))
    )
  }

  implicit lazy val arbitraryString: Arbitrary[String] = Arbitrary(
    //Gen.listOf1(arbitrary[Char] | lowAsciiChar).map(_.mkString)
    //Gen.listOf1(lowAsciiChar).map(_.mkString)
    lowAsciiChar.map(_.toString)
  )

  val lowAsciiChar: Gen[Char] = Gen.oneOf((33 to 126).map(_.toChar))

  val nothing: Gen[String] = ""

  val whitespaceChar: Gen[Char] = // ' '
    Gen.frequency((9, ' '), (1, '\t'), (2, '\n'))

  val whitespace: Gen[String] = " " //Gen.choose(1, 4).flatMap(n => Gen.listOfN(n, whitespaceChar)).map(_.mkString)

  val optionalWhitespace: Gen[String] = nothing | whitespace

  def containsAlphaNum(str: String): Boolean = str.exists(Character.isLetterOrDigit)

  def withBraces(str: Gen[String]) = str.map(x => s"[$x]")

  def withOptionalBraces(str: Gen[String]) = Gen.oneOf(str, withBraces(str))

  def withOptionalSurroundingWhitespace(gen: Gen[String]): Gen[String] = for {
    w1 <- optionalWhitespace
    mid <- gen
    w2 <- optionalWhitespace
  } yield w1 + mid + w2

  // -------------------------------------------------------------------------------------------------------------------
  // Smart Text

  def containsSpecial(s: String): Boolean = {
    lazy val s2 = removeAllWhitespace(s)
    FlowToStyle.arrowRegex.matches(s) || FlowFromStyle.arrowRegex.matches(s) || NormalisedRefRegex.matches(s2)
  }

  def doesntContainSpecial(s: String) = !containsSpecial(s)

  val plainText = arbitrary[String] suchThat doesntContainSpecial

  val optionalPlainText = nothing | plainText

  val arrowStem = Gen.choose(2, 10).map("-" * _)

  val flowFromArrow = flowAndArrow("<" + _, FlowFromStyle.unicodeArrows)
  val flowToArrow = flowAndArrow(_ + ">", FlowToStyle.unicodeArrows)

  def flowAndArrow(stemMap: String => String, unicodeArrows: List[Char]) =
    withOptionalSurroundingWhitespace(Gen.oneOf(
      arrowStem.map(_ + ">"),
      Gen.oneOf(unicodeArrows.map(_.toString))
    ))

  def flowAndRefs(arrow: Gen[String], refs: Gen[String]) = for {a <- arrow; r <- refs} yield a + r

  //  val refNum = Gen.chooseNum(0, MaxStepsPerLevel).map(_.toString)
  //  val refLetter = for {
  //    strlen <- Gen.frequency((5, 1), (2, 1), (1, 3))
  //    chars <- Gen.pick(strlen, 'a' to 'z')
  //  } yield chars.mkString
  //  val romanNumeralChars = Gen.frequency((50, 'i'), (20, 'v'), (10, 'x'), (1, 'c'), (1, 'l'))
  //  val refRoman = for {
  //    strlen <- Gen.oneOf((1 to 9).toList ::: (1 to 4).toList)
  //    chars <- Gen.listOfN(strlen, romanNumeralChars)
  //  } yield chars.mkString
  //  val refSep = withOptionalSurroundingWhitespace(".")
  //  val refInner = Gen.someOf(refNum, refLetter, refRoman).flatMap(components => components.mkString())

  // -------------------------------------------------------------------------------------------------------------------
  // Step tree

  /** A node in a label-only tree structure. (Eg. "2.0.4.b.iv") */
  case class StepPlaceholderNode(label: String, children: List[StepPlaceholderNode]) extends TreeNodeLike[StepPlaceholderNode]

  /** A tree structure of labels without any contents. */
  case class StepPlaceholderTree(override val nodes: List[StepPlaceholderNode], sli: StartingLabelIndices) extends TreeRoot[StepPlaceholderNode] {
    lazy val labels = mapRecursive(_.label)

//    lazy val stepStateTree = NormalisedStepTree(
//      convertNodeTree[StepPlaceholderNode, NormalisedStep](nodes
//      , {case (node, level, index, children) => NormalisedStep(node.label.replace('.', '_').asLocalId, "".hasNormalisedRefs, children)}
//      , sli.startingLabelIndex _
//      )
//    )
  }

  private def numberOfSteps(startingIndex: Int): Gen[Int] = {
    val max = MaxStepsPerLevel + 1 - startingIndex
    Gen.frequency(
      (80, Gen.choose(1, 4))
      , (80, Gen.choose(0, 3))
      //      ,(1, Gen.value(max))
    )
  }

  def stepPlaceholderTree(prefix: String, sli: StartingLabelIndices): Gen[StepPlaceholderTree] = {
    def go(prefix: String, level: Int, labels: List[LabelMaker]): Gen[List[StepPlaceholderNode]] =
      labels match {
        case Nil => Nil
        case labelMaker :: nextLabels =>
          numberOfSteps(sli.startingLabelIndex(level)).flatMap(size => {
            val listOfGens = (0 to (size - 1)).toList.map(i => {
              val ind = i + sli.startingLabelIndex(level)
              val lbl = prefix + labelMaker(ind)
              go(lbl + ".", level + 1, nextLabels).map(StepPlaceholderNode(lbl, _))
            })
            Gen.sequence[List, StepPlaceholderNode](listOfGens)
          })
      }
    go(prefix, 0, LabelMakerList).map(StepPlaceholderTree(_, sli))
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Data that depends on valid step refs

  class RefDependentGen(steps: StepPlaceholderTree) {

    val validRef = Gen.oneOf(steps.labels)

    val validRefWithBraces = withBraces(validRef)

    val validRefElem = withOptionalSurroundingWhitespace(validRef | validRefWithBraces)

    val validRefList: Gen[String] = for {
      lead <- optionalWhitespace
      ref <- validRefElem
      more <- Gen.listOf(validRefElem | whitespace)
    } yield lead + (ref +: more).mkString(" ")

    val flowToRefClause = flowAndRefs(flowToArrow, validRefList)
    val flowFromRefClause = flowAndRefs(flowFromArrow, validRefList)

    val textFieldText = Gen.listOf(nothing | plainText | validRefWithBraces).map(_.mkString)

    val stepText = for {
      desc <- textFieldText
      flowFrom <- (flowFromRefClause | nothing)
      flowTo <- (flowToRefClause | nothing)
      flowClauseOrder <- arbitrary[Boolean]
      flowClauseSep <- optionalWhitespace
      flow = if (flowClauseOrder) flowFrom + flowClauseSep + flowTo else flowTo + flowClauseSep + flowFrom
      clauseSep <- optionalWhitespace
    } yield desc + clauseSep + flow
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Use Case

  val useCaseTitle = arbitrary[String]

  val useCaseNumber = arbitrary[Short] suchThat (_ > 0)

  /*
  val useCase: Gen[UseCase] = {
    implicit val reactor = NoReaction
    for {
      title <- useCaseTitle
      num <- useCaseNumber
      ncac <- stepPlaceholderTree(NCAC_LabelPrefix(num), NCAC_StartingLabelIndices)
      ec <- stepPlaceholderTree(EC_LabelPrefix(num), EC_StartingLabelIndices)
      steps = StepPlaceholderTree(ncac.nodes ::: ec.nodes, null)
      uc = new UseCaseCtx(null)
      refdep = new RefDependentGen(steps)
      textFields <- Gen.listOfN(uc.textFields.size, refdep.textFieldText)
      stepTextFields <- Gen.listOfN(steps.sizeRecursive, refdep.stepText)
    } yield {
      trace(s"Creating UC with ${steps.sizeRecursive} steps.")

      // Basics
      uc.title = title
      uc.number = num

      // Tree structure
      val ncacField = uc.ncacField.get
      val ecField = uc.ecField.get
      ncacField.setState(ncac.courseFieldState)
      ecField.setState(ec.courseFieldState)
      uc.stepLabelMap.invalidate
      assume(uc.stepLabelMap.get.bs == steps.labels.toSet)

      // Text fields
      uc.textFields.zip(textFields).foreach {
        case (f, text) =>
          trace(s"t> text[${f.fd.title}] << ${text.replace("\n", "\\n")}")
          f.value.setTextFromUser(text)
          trace(s"t> text[${f.fd.title}] >> ${f.value.text.replace("\n", "\\n")}")
      }

      // Step text
      val totalStepTree = TreeLike(ncacField.courses.nodes ::: ecField.courses.nodes)
      val stepFields = ncacField.test__textFields ++ ecField.test__textFields
      val stepIt = stepTextFields.iterator
      totalStepTree.foreachRecursive(node => {
        val txt = stepIt.next
        trace(s"  step[${node.id}] << ${txt.replace("\n", "\\n")}")
        stepFields(node.id).setTextFromUser(txt)
        val after = stepFields(node.id).text
        if (txt.contains("⬅")) require(after.contains("⬅"), s"Left-flow lost!\nWas: $txt\nNow: $after")
        trace(s"  step[${node.id}] >> ${after.replace("\n", "\\n")}")
      })

      uc
    }
  }

  implicit lazy val arbUseCase: Arbitrary[UseCase] = Arbitrary(useCase)
  */
}
