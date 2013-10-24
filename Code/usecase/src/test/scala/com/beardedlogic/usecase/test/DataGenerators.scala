package com.beardedlogic.usecase
package test

import net.liftweb.common.Logger
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Prop, Arbitrary, Gen}
import scala.annotation.tailrec
import scala.util.matching.Regex
import scalaz.Memo

import db.{UseCaseHeader, FieldListRec}
import lib.Misc.removeAllWhitespace
import lib.Types._
import feature.InputValidator
import feature.uc._
import feature.uc.change.{UseCaseUpdater, NoChange}
import feature.uc.field._
import feature.uc.step._
import text.{StepText, FreeText}
import StepFieldConsts.StartingLabelIndices
import StepLabels._
import text.ParsingConfig._
import TreeOps._
import UseCaseFns.generateStepAndLabelBiMap

object DataGenerators extends Logger {
  import TestHelpers._

  implicit class RegexExt(val x: Regex) extends AnyVal {
    def matches(str: String) = x.pattern.matcher(str).matches()
  }

  private val LabelMakerList = LabelMakers.toList

  implicit class BooleanExt(val x: Boolean) extends AnyVal {
    def :||(err: => String): Prop = Prop(x) :| (if (x) "" else err)
  }

  /**
   * Same as `Gen.frequency` except if the selected generator cannot provide, it is removed from the freq map and we
   * try again.
   */
  def frequencyTrialAndError[I, O](gs: (Int, Gen[I])*)(eval: I => O)(test: O => Boolean): Gen[(I, O)] = {
    Gen(prms => {
      @tailrec def go(remaining: List[(Int, Gen[I])]): Option[(I, O)] = remaining match {
        case Nil => None
        case _ =>
          val gen = Gen.frequency(gs: _*)
          gen(prms).map(i => (i, eval(i))) match {
            case r@Some((i, o)) if test(o) => r
            case _ => go(remaining.filterNot(_._2 eq gen))
          }
      }
      go(gs.toList)
    })
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

  def withBraces(str: Gen[String]) = withOptionalSurroundingWhitespace(str).map(s => s"[$s]")

  def withOptionalBraces(str: Gen[String]) = Gen.oneOf(str, withBraces(str))

  def withOptionalSurroundingWhitespace(gen: Gen[String]): Gen[String] = for {
    w1 <- optionalWhitespace
    mid <- gen
    w2 <- optionalWhitespace
  } yield w1 + mid + w2

  def mkStringWithGen(gxs: Gen[List[String]], sep: Gen[String]): Gen[String] =
    gxs.flatMap(xs =>
      xs.foldRight(Gen.value("")) {
        case (a, g) =>
          for {b <- g; s <- sep} yield a + s + b
      })

  def mkStringWithWhitespace(gxs: Gen[List[String]]): Gen[String] = mkStringWithGen(gxs, whitespace)

  def maybe[T](g: Gen[T]): Gen[Option[T]] = g.map(Some(_)) | None

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

  val randomStepLabelWithoutPrefix = {
    val refNum = Gen.chooseNum(0, MaxStepsPerLevel).map(_.toString)
    val refLetter = for {
      strlen <- Gen.frequency((85, 1), (12, 2), (3, 3))
      chars <- Gen.pick(strlen, 'a' to 'z')
    } yield chars.mkString
    val romanNumeralChar = Gen.frequency((50, 'i'), (20, 'v'), (10, 'x'), (1, 'c'), (1, 'l'))
    val refRoman = for {
      strlen <- Gen.oneOf((1 to 9).toList ::: (1 to 4).toList)
      chars <- Gen.listOfN(strlen, romanNumeralChar)
    } yield chars.mkString
    val sep = withOptionalSurroundingWhitespace(".")
    val levels = List(refNum, refLetter, refRoman, refNum)
    val stepComponents: Gen[List[String]] = Gen.choose(1, levels.size).flatMap(sz => Gen.sequence[List, String](levels.take(sz)))

    mkStringWithGen(stepComponents, sep)
  }

  def randomStepLabelForUc(n: UseCaseNumber) = for {
    prefix <- Gen.value(s"$n.") | s"$n.E."
    suffix <- randomStepLabelWithoutPrefix
  } yield prefix + suffix

  def randomStepLabel = Gen.chooseNum(1,100).flatMap(n => randomStepLabelForUc(n.toShort.tag[IsUseCaseNumber]))

  // -------------------------------------------------------------------------------------------------------------------
  // Step tree

  /** A node in a label-only tree structure. (Eg. "2.0.4.b.iv") */
  case class StepPlaceholderNode(label: StepLabel, children: List[StepPlaceholderNode]) extends TreeNodeLike[StepPlaceholderNode]

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
              val lbl = (prefix + labelMaker(ind)).asLabel
              go(lbl + ".", level + 1, nextLabels, 0).map(StepPlaceholderNode(lbl, _))
            })
            Gen.sequence[List, StepPlaceholderNode](listOfGens)
          })
      }
    go(prefix, 0, LabelMakerList, minSteps).map(StepPlaceholderTree(_, sli))
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Data that depends on valid step refs

  object UcDataGenCtx {
    def apply(tree: StepPlaceholderTree, selfUcNum: UseCaseNumber, ucsInProject: Seq[UseCaseNumber]) =
      new UcDataGenCtx(tree.labels, selfUcNum, ucsInProject)

    def apply(uc: UseCase, ucsInProject: Seq[UseCaseNumber]) =
      new UcDataGenCtx(uc.stepsAndLabels.value.bs.toSeq, uc.number, ucsInProject)

    def apply(c: UcParsingCtx, ucsInProject: Seq[UseCaseNumber]) =
      new UcDataGenCtx(c.stepsAndLabels.value.bs.toSeq, c.ucn, ucsInProject)
  }

  class UcDataGenCtx(validSteps: Seq[StepLabel], selfUcNum: UseCaseNumber, existingUcs: Seq[UseCaseNumber]) {
    val ucsInProject = (selfUcNum :: existingUcs.toList).distinct
    val maxUcn = ucsInProject.map(_.toInt).max

    val validStep = Gen.oneOf(validSteps)
    val invalidStep = randomStepLabel suchThat (x => !validSteps.contains(removeAllWhitespace(x)))
    val validStepRef = withBraces(validStep)

    val possibleUcRefTitleSuffix = (nothing | useCaseTitle.map(":" + _))
    val validUcRefInner = for {
      prefix <- Gen.oneOf("uc","UC","uc-","UC-")
      n      <- Gen.oneOf(ucsInProject)
      title  <- possibleUcRefTitleSuffix
    } yield prefix + n + title

    val invalidUcRefInner = possibleUcRefTitleSuffix.map(s"UC-${maxUcn + 1}?" + _)

    val textRef = withBraces(validStep | invalidStep | validUcRefInner | invalidUcRefInner | DeletedRefInner)

    val textFieldText = Gen.listOf(nothing | plainText | textRef).map(_.mkString)

    val validFlowRef = validStep | validStepRef
    val validFlowRefs = mkStringWithWhitespace(Gen.listOf(validFlowRef))
    val flowToRefClause = flowAndRefs(flowToArrow, validFlowRefs)
    val flowFromRefClause = flowAndRefs(flowFromArrow, validFlowRefs)

    // TODO StepText now supports arbitrary flow clause count
    val stepText = for {
      desc <- textFieldText
      clauseSep <- optionalWhitespace
      flowFrom <- (flowFromRefClause | nothing)
      flowTo <- (flowToRefClause | nothing)
      flowClauseOrder <- arbitrary[Boolean]
      flowClauseSep <- optionalWhitespace
      flow = if (flowClauseOrder) flowFrom + flowClauseSep + flowTo else flowTo + flowClauseSep + flowFrom
    } yield desc + clauseSep + flow
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Step Field Values

  case class NcSfv(h: UseCaseHeader, sfv: StepFieldValue, stepsAndLabels: StepAndLabelBiMap)

  def stepFieldValue(stepTexts: List[String], f: StepField, tree: StepTree)(implicit ctx: UcParsingCtx): StepFieldValue = {
    val textIter = stepTexts.iterator
    val textmap = tree.mapRecursive(s => {
      val txt = textIter.next
      //trace(s"  step[${node.id}] << ${txt.replace("\n", "\\n")}")
      val v = StepText.parse(txt)
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
      refdep = UcDataGenCtx(steps, ucn, List.empty)
      stepTexts <- Gen.listOfN(steps.sizeRecursive, refdep.stepText)
    } yield {
      val stepsAndLabels = generateStepAndLabelBiMap(ucn, (ncf -> nc.stepTree))
      val ctx = UcParsingCtx(ucn, uch.title, stepsAndLabels, UseCaseRelations.Empty)
      val sfv = stepFieldValue(stepTexts, ncf, nc.stepTree)(ctx)
      NcSfv(uch, sfv, stepsAndLabels)
    }
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Use Case

  private val usecasetitleRemoval = "[\\[\\]⦋⦌［］]+".r

  lazy val useCaseTitle = arbitrary[String]
                     .map(usecasetitleRemoval.replaceAllIn(_, ""))
                     .map(AnyValidArrowRegex.replaceAllIn(_, ""))
                     .map(s => if (s.isEmpty) "X" else s)
                     .map(InputValidator.useCaseTitle.correctAndValidate(_).toOption)
                     .suchThat(_.isDefined)
                     .map(_.get)

  val useCaseNumber = Gen.posNum[Short].map(_.tag[IsUseCaseNumber])

  val useCaseHeader = for {
    title <- useCaseTitle
  } yield UseCaseHeader(title)

  implicit val arbUCH = Arbitrary(useCaseHeader)
  implicit val arbUCN = Arbitrary(useCaseNumber)

  def useCaseGen(fieldList_ : => FieldListRec, ucnGen: Gen[UseCaseNumber] = useCaseNumber, existingUcs: Seq[UseCaseNumber] = List.empty): Gen[UseCase] = {
    val fieldList = fieldList_
    val NCF = fieldList.NCF
    val ECF = fieldList.ECF

    for {
      h <- arbitrary[UseCaseHeader]
      ucn <- ucnGen
      nc <- stepPlaceholderTree(NCF.rootLabelPrefix(ucn),  NCF.sli, 1)
      ec <- stepPlaceholderTree(ECF.rootLabelPrefix(ucn),  ECF.sli, 0)
      steps = StepPlaceholderTree(nc.nodes ::: ec.nodes, null)
      refdep = UcDataGenCtx(steps, ucn, existingUcs)
      textFieldTexts <- Gen.listOfN(fieldList.textFields.size, refdep.textFieldText)
      stepTexts <- Gen.listOfN(steps.sizeRecursive, refdep.stepText)
    } yield {
      trace(s"Creating UC with ${steps.sizeRecursive} steps.")

      // Parsing context
      implicit val stepsAndLabels = generateStepAndLabelBiMap(ucn, (NCF -> nc.stepTree), (ECF -> ec.stepTree))
      assume(stepsAndLabels.value.bs == steps.labels.toSet)
      implicit val ctx = UcParsingCtx(ucn, h.title, stepsAndLabels, UseCaseRelations.Empty)

      // Text fields
      val textFieldValues: FieldValues = fieldList.textFields.zip(textFieldTexts).map {
        case (f, text) =>
          //trace(s"t> text[${f.defn.title}] << ${text.replace("\n", "\\n")}")
          val v = FreeText.parse(text)
          //trace(s"t> text[${f.defn.title}] >> ${v.text.replace("\n", "\\n")}")
          (f ~> v)
        }.toMap

      val fieldValues: FieldValues =
        fieldList.fields.map(ff => {
          val r: (Field, Field#Value) = ff match {
            case f: NormalCourseField    => (f ~> stepFieldValue(stepTexts, f, nc.stepTree))
            case f: ExceptionCourseField => (f ~> stepFieldValue(stepTexts, f, ec.stepTree))
            case f: TextField            => (f -> textFieldValues(f))
            case f: FlowGraphField       => (f ~> f.empty)
          }
          r
        }).toMap
      UseCase(ucn, h, fieldList.fields, fieldValues, stepsAndLabels)
    }
  }

  // -------------------------------------------------------------------------------------------------------------------
  // Use Case mutation

  type UcMutationResult = (UcUpdateResult, String)
  private val EmptyUcMutationResult: UcMutationResult = (NoChange, "NoChange")

  implicit def ucTc(uc: UseCase) = UcParsingCtx(uc, UseCaseRelations.Empty)
  implicit def uTuc(u: UseCaseUpdater) = u.uc

  case class UseCaseMutator(newFn: UseCaseUpdater => UcMutationResult) {
    val cache = Memo.immutableListMapMemo[UseCase, UcMutationResult](uc => newFn(UseCaseUpdater(uc, UseCaseRelations.Empty)))
    def apply(uc: UseCase) = cache(uc)
    override def toString = s"UseCaseMutator@${hashCode.toHexString}"
  }

  object UseCaseMutators {

    private def findChangableStep(sfv: StepFieldValue, eval: LocalStepId => UcUpdateResult): Option[(LocalStepId, UcUpdateResult)] =
      findTransformable(sfv.textmap.keys.toIndexedSeq, eval)(_.getChanges.nonEmpty)

    private def fieldMutator(fn: (UseCaseUpdater, UcDataGenCtx) => Gen[Option[UcMutationResult]]): Gen[UseCaseMutator] =
      Gen(prms => Some(
        UseCaseMutator(uc => {
          val refdep = UcDataGenCtx(uc, List.empty) // TODO Field mutators aren't aware of other UCs
          val g = fn(uc, refdep)
          g.apply(prms).flatten.getOrElse(EmptyUcMutationResult)
        })))

    private def mutateStepNoText(fn: (StepField, LocalStepId) => UseCaseUpdater => UcUpdateResult, desc: (UseCase, StepField, LocalStepId) => String): Gen[UseCaseMutator] =
      fieldMutator((u, refdep) => {
        val uc = u.uc
        val perField = UseCaseFns.filter[StepField](uc.fields).toStream.map(f =>
          for ((id, r) <- findChangableStep(f(uc.fieldValues), fn(f, _)(u))) yield (r, desc(uc, f, id))
        )
        val result = perField.filter(_.isDefined).headOption.flatten
        result
      })

    private def mutateStep(fn: (StepField, LocalStepId, String) => UseCaseUpdater => UcUpdateResult, desc: (UseCase, StepField, LocalStepId, String) => String): Gen[UseCaseMutator] =
      fieldMutator((u, refdep) => {
        val uc = u.uc
        refdep.stepText.flatMap(txt => {
          val perField = UseCaseFns.filter[StepField](uc.fields).toStream.map(f =>
            for ((id, r) <- findChangableStep(f(uc.fieldValues), fn(f, _, txt)(u))) yield (r, desc(uc, f, id, txt))
          )
          val result = perField.filter(_.isDefined).headOption.flatten
          result
        })})

    // -----------------------------------------------------
    // Mutation implementations

    val MutateTitle =
      for (title <- useCaseTitle)
      yield UseCaseMutator(u => (u.updateTitle(title), s"[/] Change UC title to ${title.inspect}"))

    val MutateTextField = fieldMutator((u, refdep) =>
      for {
        f <- Gen.oneOf(UseCaseFns.filter[TextField](u.uc.fields))
        txt <- refdep.textFieldText
      } yield
        Some(f.updateText(txt)(u), s"[=] Change TextField [${f.defn.title}] to ${txt.inspect}")
    )

    val AddTailStep = fieldMutator((u, refdep) =>
      for (f <- Gen.oneOf(UseCaseFns.filter[StepField](u.uc.fields)))
      yield Some(f.addTailStep(u),s"[+] Add tail step to ${f.getClass.getSimpleName}")
    )

    val MutateStepText = mutateStep(_.updateText(_, _), (uc, f, id, txt) => s"[=] Change step text ${id.withLabel(uc)} to ${txt.inspect}")
    val AddStep = mutateStepNoText(_.addStep(_), (uc, _, id) => s"[+] Add step ${id.withLabel(uc)}")
    val RemoveStep = mutateStepNoText(_.removeStep(_), (uc, _, id) => s"[-] Remove step ${id.withLabel(uc)}")
    val DecreaseIdent = mutateStepNoText(_.decreaseIndent(_), (uc, _, id) => s"[<] Dec step ${id.withLabel(uc)}")
    val IncreaseIdent = mutateStepNoText(_.increaseIndent(_), (uc, _, id) => s"[>] Inc step ${id.withLabel(uc)}")
  }

  val useCaseMutator: Gen[UseCaseMutator] = {
    import UseCaseMutators._
    Gen(prms => Some(UseCaseMutator(uc => {
      val x = frequencyTrialAndError(
        (1, MutateTitle)
        , (30, MutateTextField)
        , (60, MutateStepText)
        , (14, AddStep)
        , (16, AddTailStep)
        , (10, RemoveStep)
        , (10, DecreaseIdent)
        , (10, IncreaseIdent)
      )(_.apply(uc))(_._1.getChanges.nonEmpty)
      val result: UcMutationResult = x(prms).map(_._2).getOrElse(EmptyUcMutationResult)
      result
    })))
  }

  implicit lazy val arbUseCaseMutator: Arbitrary[UseCaseMutator] = Arbitrary(useCaseMutator)
}
