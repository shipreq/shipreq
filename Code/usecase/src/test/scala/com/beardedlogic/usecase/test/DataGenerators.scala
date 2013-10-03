package com.beardedlogic.usecase
package test

import net.liftweb.common.Logger
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Prop, Arbitrary, Gen}
import scala.util.matching.Regex

import lib._
import field._
import tree._
import change.NoChange
import db.{UseCaseHeader, FieldListRec}
import text.{StepText, FreeText}
import Misc.removeAllWhitespace
import StepField.StartingLabelIndices

import TestHelpers.AnyExt
import StepLabels._
import text.ParsingConfig._
import TreeOps._
import Types._
import UseCaseFns.generateStepAndLabelBiMap

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

    lazy val stepTree = StepTree(
      convertNodeTree[StepPlaceholderNode, StepNode](nodes
      , {case (node, level, index, children) => StepNode(node.label.replace('.', '_').asLocalStepId, level, index, children)}
      , sli.startingLabelIndex _
      )
    )
  }

  private def numberOfSteps(minSteps: Int, startingIndex: Int): Gen[Int] = {
    //val max = MaxStepsPerLevel + 1 - startingIndex
    Gen.frequency(
      (100, Gen.choose(minSteps, 3))
      ,(3, Gen.choose(minSteps, 20))
      //,(1, Gen.value(max))
    )
  }

  def stepPlaceholderTree(prefix: String, sli: StartingLabelIndices, minSteps: Int): Gen[StepPlaceholderTree] = {
    def go(prefix: String, level: Int, labels: List[LabelMaker], minSteps: Int): Gen[List[StepPlaceholderNode]] =
      labels match {
        case Nil => Nil
        case labelMaker :: nextLabels =>
          numberOfSteps(minSteps, sli.startingLabelIndex(level)).flatMap(size => {
            val listOfGens = (0 to (size - 1)).toList.map(i => {
              val ind = i + sli.startingLabelIndex(level)
              val lbl = prefix + labelMaker(ind)
              go(lbl + ".", level + 1, nextLabels, 0).map(StepPlaceholderNode(lbl, _))
            })
            Gen.sequence[List, StepPlaceholderNode](listOfGens)
          })
      }
    go(prefix, 0, LabelMakerList, minSteps).map(StepPlaceholderTree(_, sli))
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Data that depends on valid step refs

  object RefDependentGen {
    def apply(tree: StepPlaceholderTree) = new RefDependentGen(tree.labels)
    def apply(uc: UseCase) = new RefDependentGen(uc.stepsAndLabels.value.ba.keys.toSeq)
  }

  class RefDependentGen(labels: Seq[String]) {

    val validRef = Gen.oneOf(labels)

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
  // Step Field Values

  case class NcSfv(h: UseCaseHeader, sfv: StepFieldValue, stepsAndLabels: StepAndLabelBiMap)

  def stepFieldValue(stepTexts: List[String], f: StepField, tree: StepTree)(implicit sl: StepAndLabelBiMap): StepFieldValue = {
    val textIter = stepTexts.iterator
    val textmap = tree.mapRecursive(s => {
      val txt = textIter.next
      //trace(s"  step[${node.id}] << ${txt.replace("\n", "\\n")}")
      val v = StepText.parse(s.id, txt)
      //val after = stepFields(node.id).text
      //if (txt.contains("⬅")) require(after.contains("⬅"), s"Left-flow lost!\nWas: $txt\nNow: $after")
      //trace(s"  step[${node.id}] >> ${after.replace("\n", "\\n")}")
      (s.id -> v)
    }).toMap
    StepFieldValue(f, tree, textmap)
  }

  def genNcSfv(NCF: => NormalCourseField): Gen[NcSfv] = {
    val ncf = NCF
    for {
      uch <- arbitrary[UseCaseHeader]
      ucn <- arbitrary[UseCaseNumber]
      nc <- stepPlaceholderTree(ncf.rootLabelPrefix(ucn), ncf.sli, 1)
      steps = StepPlaceholderTree(nc.nodes, null)
      refdep = RefDependentGen(steps)
      stepTexts <- Gen.listOfN(steps.sizeRecursive, refdep.stepText)
    } yield {
      implicit val stepsAndLabels = generateStepAndLabelBiMap(ucn, (ncf -> nc.stepTree))
      val sfv = stepFieldValue(stepTexts, ncf, nc.stepTree)
      NcSfv(uch, sfv, stepsAndLabels)
    }
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Use Case

  val useCaseTitle = arbitrary[String]

  val useCaseNumber = Gen.posNum[Short].map(_.tag[UseCaseNumberTag])

  val useCaseHeader = for {
    title <- useCaseTitle
  } yield UseCaseHeader(title)

  implicit val arbUCH = Arbitrary(useCaseHeader)
  implicit val arbUCN = Arbitrary(useCaseNumber)

  def useCaseGen(fieldList: => FieldListRec, ucnGen: Gen[UseCaseNumber] = useCaseNumber): Gen[UseCase] = {
    val NCF = fieldList.NCF
    val ECF = fieldList.ECF

    for {
      h <- arbitrary[UseCaseHeader]
      ucn <- ucnGen
      nc <- stepPlaceholderTree(NCF.rootLabelPrefix(ucn),  NCF.sli, 1)
      ec <- stepPlaceholderTree(ECF.rootLabelPrefix(ucn),  ECF.sli, 0)
      steps = StepPlaceholderTree(nc.nodes ::: ec.nodes, null)
      refdep = RefDependentGen(steps)
      textFieldTexts <- Gen.listOfN(fieldList.textFields.size, refdep.textFieldText)
      stepTexts <- Gen.listOfN(steps.sizeRecursive, refdep.stepText)
    } yield {
      trace(s"Creating UC with ${steps.sizeRecursive} steps.")

      // Steps and Labels
      implicit val stepsAndLabels = generateStepAndLabelBiMap(ucn, (NCF -> nc.stepTree), (ECF -> ec.stepTree))
      assume(stepsAndLabels.value.bs == steps.labels.toSet)

      // Text fields
      val textFieldValues: FieldValues = fieldList.textFields.zip(textFieldTexts).map {
        case (f, text) =>
          //trace(s"t> text[${f.defn.title}] << ${text.replace("\n", "\\n")}")
          val v = FreeText.parse(text)
          //trace(s"t> text[${f.defn.title}] >> ${v.text.replace("\n", "\\n")}")
          (f ~> v)
        }.toMap

      // Step text
      val stepFieldValues: FieldValues = Map(
        (NCF ~> stepFieldValue(stepTexts, NCF, nc.stepTree)),
        (ECF ~> stepFieldValue(stepTexts, ECF, ec.stepTree))
      )

      val fieldValues = stepFieldValues ++ textFieldValues
      UseCase(ucn, h, fieldList.fields, fieldValues, stepsAndLabels)
    }
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Use Case mutation

  case class UseCaseMutator(name: String, fn: UseCase => UcUpdateResult) {
    def apply(uc: UseCase) = fn(uc)
    override def toString = s"UseCaseMutator($name)"
  }

  object UseCaseMutators {

    private def mutateField(name: String, fn: (UseCase, RefDependentGen) => Gen[UcUpdateResult]): Gen[UseCaseMutator] = Gen(prms =>
      Some(UseCaseMutator(name, uc => {
        val refdep = RefDependentGen(uc)
        val g = fn(uc, refdep)
        g.apply(prms).getOrElse(NoChange)
      })))

    private def mutateStepNoText(name: String, fn: (StepField, LocalStepId) => UseCase => UcUpdateResult) =
      mutateField(name, (uc, refdep) =>
        for {
          f <- Gen.oneOf(UseCaseFns.filter[StepField](uc.fields))
          id <- Gen.oneOf(f(uc.fieldValues).textmap.keys.toSeq)
        } yield fn(f, id)(uc)
      )

    private def mutateStep(name: String, fn: (StepField, LocalStepId, String) => UseCase => UcUpdateResult) =
      mutateField(name, (uc, refdep) =>
        for {
          f <- Gen.oneOf(UseCaseFns.filter[StepField](uc.fields))
          id <- Gen.oneOf(f(uc.fieldValues).textmap.keys.toSeq)
          txt <- refdep.stepText
        } yield fn(f, id, txt)(uc)
      )

    val MutateTitle = for (title <- useCaseTitle) yield UseCaseMutator("Title", _.updateTitle(title))

    val MutateTextField = mutateField("TextField", (uc, refdep) =>
      for {
        f <- Gen.oneOf(UseCaseFns.filter[TextField](uc.fields))
        txt <- refdep.textFieldText
      } yield f.updateText(txt)(uc)
    )

    val MutateStepText = mutateStep("StepText", (f, id, txt) => f.updateText(id, txt))
    val AddStep = mutateStepNoText("AddStep", (f, id) => f.addStep(id))
    val RemoveStep = mutateStepNoText("RemoveStep", (f, id) => f.removeStep(id))
    val DecreaseIdent = mutateStepNoText("DecreaseIdent", (f, id) => f.decreaseIndent(id))
    val IncreaseIdent = mutateStepNoText("IncreaseIdent", (f, id) => f.increaseIndent(id))
  }

  val useCaseMutator: Gen[UseCaseMutator] = {
    import UseCaseMutators._
    Gen.frequency(
      (1, MutateTitle)
      , (10, MutateTextField)
      , (50, MutateStepText)
      , (15, AddStep)
      , (15, RemoveStep)
      , (10, DecreaseIdent)
      , (10, IncreaseIdent)
    )
  }

  implicit lazy val arbUseCaseMutator: Arbitrary[UseCaseMutator] = Arbitrary(useCaseMutator)
}
