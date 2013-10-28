package com.beardedlogic.usecase.feature.publish

import com.beardedlogic.usecase.feature.uc.UseCase
import com.beardedlogic.usecase.feature.uc.field._
import com.beardedlogic.usecase.feature.uc.step.{StepNode, StepTreeZipper}
import com.beardedlogic.usecase.feature.uc.text.FreeTextTerms._
import com.beardedlogic.usecase.feature.uc.text.{FlowClause, FreeTextTerm, StepText, FreeText}
import com.beardedlogic.usecase.lib.Types._
import net.liftweb.util.TimeHelpers.logTime
import scalaz.{Name, Need, Traverse, Monoid}
import scalaz.syntax.foldable._
import scalaz.syntax.monoid._
import com.beardedlogic.usecase.feature.FlowGraph

// =====================================================================================================================

sealed trait LogicalField
case class LogicalLastUpdatedField(when: String @@ ISO8601) extends LogicalField
case class LogicalFlowGraphField(dot: Name[String]) extends LogicalField
case class LogicalTextField(title: String, value: FreeText) extends LogicalField
case class LogicalStepField(title: String, value: Option[StepTreeZipper.DeepZipper]) extends LogicalField

// =====================================================================================================================

abstract class GenericPublisher(input: Input) {

  type X
  implicit def xMonoid: Monoid[X]
  implicit def listMonoid[A]: Monoid[List[A]] = scalaz.std.list.listMonoid
  implicit def listInstance: Traverse[List] = scalaz.std.list.listInstance

  @inline final def zero = xMonoid.zero

  @inline final def useCases = input.sortedUseCases

  // -------------------------------------------------------------------------------------------------------------------
  // High-level

  def doc: X = logTime(s"${getClass.getSimpleName}.doc(${useCases.size} UCs)")(
    doc(toc, articles)
  )
  def doc(toc: X, articles: X): X = toc |+| articles

  def toc: X = tocSurround(useCases foldMap tocEntry)
  def tocSurround(entries: X): X
  def tocEntry(uc: UseCase): X

  def articles: X = articles(useCases map article)
  def articles(articles: List[X]): X = articles concatenate
  def article(uc: UseCase): X = article(uc, fields(uc))
  def article(uc: UseCase, fields: X): X

  // -------------------------------------------------------------------------------------------------------------------
  // Fields

  def fields(uc: UseCase): X =
    getLogicalFields(uc) foldMap field

  def getLogicalFields(uc: UseCase): List[LogicalField] =
    LogicalLastUpdatedField(input.revMap(uc).createdAt) ::
    uc.fields.foldMap(extractLogicalFields(uc))

  def extractLogicalFields(uc: UseCase)(ff: Field): List[LogicalField] =
    ff match {
      case f: TextField =>
        LogicalTextField(f.defn.title, f(uc)) :: Nil

      case f: NormalCourseField =>
        val v = f(uc)
        val n :: a = v.tree.nodes
        List(
          LogicalStepField("Normal Course", buildStepTreeZipper(uc, v, n :: Nil)),
          LogicalStepField("Alternative Courses", buildStepTreeZipper(uc, v, a))
        )

      case f: ExceptionCourseField =>
        val v = f(uc)
        LogicalStepField("Exceptions", buildStepTreeZipper(uc, v, v.tree.nodes)) :: Nil

      case f: FlowGraphField =>
        LogicalFlowGraphField(Need(FlowGraph.render(uc).toString)) :: Nil
    }

  def buildStepTreeZipper(uc: UseCase, v: StepFieldValue, nodes: List[StepNode]): Option[StepTreeZipper.DeepZipper] =
    nodes match {
      case Nil => None
      case h :: t =>
        val labels = uc.stepsAndLabels.value.ab
        val zipper = StepTreeZipper.DeepBuilder(v.textmap, labels).build(h, t)
        Some(zipper)
    }

  def field(f: LogicalField): X = f match {
    case f: LogicalTextField        => textField(f)
    case f: LogicalStepField        => stepField(f)
    case f: LogicalFlowGraphField   => flowGraphField(f)
    case f: LogicalLastUpdatedField => lastUpdatedField(f)
  }

  def fieldTitle(title: String): X

  // -------------------------------------------------------------------------------------------------------------------
  // Text field

  def textField(f: LogicalTextField): X = textFieldSurround(textFieldTitle(f.title), textFieldValue(f.value))
  def textFieldSurround(title: X, value: X): X

  final def textFieldTitle(title: String): X = textFieldTitleSurround(textFieldTitleInner(title))
  def textFieldTitleSurround(title: X): X
  def textFieldTitleInner(title: String): X = fieldTitle(title)

  final def textFieldValue(value: FreeText): X = textFieldValueSurround(textFieldValueInner(value))
  def textFieldValueSurround(value: X): X
  def textFieldValueInner(value: FreeText): X = text(value)

  final def text(value: FreeText): X =
    markupTokens(
      TextMarkup.markup(
        TextMarkup.introduce(
          value.terms)))

  final def markupTokens(tokens: List[MarkupToken]): X = {
    var prev: MarkupToken = null
    tokens.foldLeft(zero)((acc, mt) => {
      val x = markupToken(mt)
      val r =
        if (prev eq null)
          x
        else
          acc |+| betweenMarkupTokens(prev, mt) |+| x
      prev = mt
      r
    })
  }

  def markupToken(t: MarkupToken): X
  def betweenMarkupTokens(a: MarkupToken, b: MarkupToken): X

  final def term(term: FreeTextTerm): X = term match {
    case t@PlainText(_)            => fttPlainText(t)
    case t@StepRef(_, _)           => fttStepRef(t)
    case t@InvalidStepRef(_)       => fttInvalidStepRef(t)
    case t@DeletedRef              => fttDeletedRef
    case t@UseCaseRef(_, _   )     => fttUseCaseRef(t)
    case t@UseCaseSelfRef(_, _)    => fttUseCaseSelfRef(t)
    case t@InvalidUseCaseRef(_, _) => fttInvalidUseCaseRef(t)
  }

  def fttPlainText(value: PlainText)                : X
  def fttDeletedRef                                 : X
  def fttStepRef(value: StepRef)                    : X
  def fttInvalidStepRef(value: InvalidStepRef)      : X
  def fttAnyUseCaseRef(value: AnyUseCaseRef)        : X
  def fttUseCaseRef(value: UseCaseRef)              : X = fttAnyUseCaseRef(value)
  def fttUseCaseSelfRef(value: UseCaseSelfRef)      : X = fttAnyUseCaseRef(value)
  def fttInvalidUseCaseRef(value: InvalidUseCaseRef): X

  // -------------------------------------------------------------------------------------------------------------------
  // Step fields

  def stepField(f: LogicalStepField): X = stepFieldSurround(stepFieldTitle(f.title), stepFieldValue(f))
  def stepFieldSurround(title: X, value: X): X

  final def stepFieldTitle(title: String): X = stepFieldTitleSurround(stepFieldTitleInner(title))
  def stepFieldTitleSurround(title: X): X
  def stepFieldTitleInner(title: String): X = fieldTitle(title)

  final def stepFieldValue(f: LogicalStepField): X = f.value match {
    case None    => stepFieldValueEmpty
    case Some(v) => stepFieldValueSurround(stepFieldValueGeneration(v))
  }
  def stepFieldValueEmpty: X
  def stepFieldValueSurround(value: X): X
  final def stepFieldValueGeneration(zipper: StepTreeZipper.DeepZipper): X =
    stepFieldValueGenerationSurround(zipper.focus.level, stepFieldValueGenerationInner(zipper))
  def stepFieldValueGenerationSurround(level: Int, generation: X): X
  def stepFieldValueGenerationInner(zipper: StepTreeZipper.DeepZipper): X =
    zipper.foldMap(n => {
      val x = stepFieldValueStep(n)
      n.down match {
        case None          => x
        case Some(nextGen) => x |+| stepFieldValueGeneration(nextGen)
      }
    })
  def stepFieldValueStep(value: StepTreeZipper.AnyFocus): X

  def stepText(value: StepText): X =
    text(value.mainClause) |+| flowClause(value.flowFromClause) |+| flowClause(value.flowToClause)

  def flowClause(oc: Option[FlowClause]): X =
    oc match {
      case None => zero
      case Some(c) => flowClause(c)
    }
  def flowClause(c: FlowClause): X

  // -------------------------------------------------------------------------------------------------------------------
  // Other fields

  def lastUpdatedField(f: LogicalLastUpdatedField): X
  def flowGraphField(f: LogicalFlowGraphField): X
}
