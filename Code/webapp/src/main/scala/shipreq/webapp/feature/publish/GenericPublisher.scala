package shipreq.webapp.feature.publish

import net.liftweb.util.TimeHelpers.logTime
import scalaz.{Need, Traverse, Monoid}

import shipreq.webapp.feature.uc.UseCase
import shipreq.webapp.feature.uc.field._
import shipreq.webapp.feature.uc.step.{StepNode, StepTreeZipper}
import shipreq.webapp.feature.uc.text.FreeTextTerms._
import shipreq.webapp.feature.uc.text.{FreeTextTerm, StepText, FreeText}
import shipreq.webapp.lib.ScalazSubset._
import shipreq.webapp.lib.Misc.DateTimeExt
import shipreq.webapp.lib.Types._
import shipreq.webapp.feature.FlowGraph
import shipreq.webapp.db.UseCaseRev

abstract class GenericPublisher(input: Input) {

  type X
  implicit def xMonoid: Monoid[X]
  implicit def listMonoid[A]: Monoid[List[A]] = scalaz.std.list.listMonoid
  implicit def listInstance: Traverse[List] = scalaz.std.list.listInstance
  implicit def markupTokenRenderer: MarkupTokenRenderer[X]
  implicit def fttRenderer: FTTRenderer[X]
  implicit def stepRenderer: StepRenderer[X]

  @inline final def zero = xMonoid.zero

  @inline final def useCases = input.sortedUseCases

  @inline final def inScope(num: UseCaseNumber) = useCases.exists(_.number == num)

  final def markedUpText(value: String)            : X = markedUpText(List(PlainText(value)))
  final def markedUpText(terms: List[FreeTextTerm]): X = Helpers.markupAndRenderFTTs(terms)

  // -------------------------------------------------------------------------------------------------------------------
  // High-level

  def doc: X = logTime(s"${getClass.getSimpleName}.doc(${useCases.size} UCs)")(
    if (useCases.isEmpty)
      optionalDocHeader
    else
      doc(optionalDocHeader, optionalDocLastUpdated, toc, articles)
  )
  def doc(header: X, lastUpdated: X, toc: X, articles: X): X =
          header |+| lastUpdated |+| toc |+| articles

  def optionalDocHeader: X = input.header.fold(docHeaderSubst)(docHeader)
  def docHeaderSubst: X = zero
  def docHeader(h: DocHeader): X = docHeader(docHeaderTitle(h.title), h.preface.fold(zero)(docHeaderPreface))
  def docHeader(title: X, preface: X): X
  def docHeaderTitle(t: String): X
  final def docHeaderPreface(p: String) = markedUpText(p)
  def docHeaderPreface(p: X): X

  def optionalDocLastUpdated: X = input.lastUpdated.map(t => docLastUpdated(t.toIso8601)) getOrElse zero
  def docLastUpdated(t: ISO8601): X

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
    getLogicalFields(uc, input.revMap(uc)) foldMap field

  def getLogicalFields(uc: UseCase, rev: UseCaseRev): List[OutputField] =
    OF_Revision(rev.rev) ::
    OF_LastUpdated(rev.createdAt) ::
    uc.fields.foldMap(extractLogicalFields(uc))

  def extractLogicalFields(uc: UseCase)(ff: Field): List[OutputField] =
    ff match {
      case f: TextField =>
        OF_Text(f.defn.title, f(uc)) :: Nil

      case f: NormalCourseField =>
        val v = f(uc)
        val n :: a = v.tree.nodes
        List(
          OF_Step("Normal Course", buildStepTreeZipper(uc, v, n :: Nil)),
          OF_Step("Alternative Courses", buildStepTreeZipper(uc, v, a))
        )

      case f: ExceptionCourseField =>
        val v = f(uc)
        OF_Step("Exceptions", buildStepTreeZipper(uc, v, v.tree.nodes)) :: Nil

      case f: FlowGraphField =>
        OF_FlowGraph(Need(FlowGraph.render(uc).toString)) :: Nil
    }

  def buildStepTreeZipper(uc: UseCase, v: StepFieldValue, nodes: List[StepNode]): Option[StepTreeZipper.DeepZipper] =
    nodes match {
      case Nil => None
      case h :: t =>
        val labels = uc.stepsAndLabels.value.ab
        val zipper = StepTreeZipper.DeepBuilder(v.textmap, labels).build(h, t)
        Some(zipper)
    }

  def field(f: OutputField): X = f match {
    case f: OF_Text        => textField(f)
    case f: OF_Step        => stepField(f)
    case f: OF_FlowGraph   => flowGraphField(f)
    case f: OF_LastUpdated => lastUpdatedField(f)
    case f: OF_Revision    => revisionField(f)
  }

  def fieldTitle(title: String): X

  // -------------------------------------------------------------------------------------------------------------------
  // Text field

  def textField(f: OF_Text): X = textFieldSurround(textFieldTitle(f.title), textFieldValue(f.value))
  def textFieldSurround(title: X, value: X): X

  final def textFieldTitle(title: String): X = textFieldTitleSurround(textFieldTitleInner(title))
  def textFieldTitleSurround(title: X): X
  def textFieldTitleInner(title: String): X = fieldTitle(title)

  final def textFieldValue(value: FreeText): X = textFieldValueSurround(textFieldValueInner(value))
  def textFieldValueSurround(value: X): X
  def textFieldValueInner(value: FreeText): X = text(value)

  final def text(value: FreeText): X = markedUpText(value.terms)

  // -------------------------------------------------------------------------------------------------------------------
  // Step fields

  def stepField(f: OF_Step): X = stepFieldSurround(stepFieldTitle(f.title), stepFieldValue(f))
  def stepFieldSurround(title: X, value: X): X

  final def stepFieldTitle(title: String): X = stepFieldTitleSurround(stepFieldTitleInner(title))
  def stepFieldTitleSurround(title: X): X
  def stepFieldTitleInner(title: String): X = fieldTitle(title)

  final def stepFieldValue(f: OF_Step): X = f.value match {
    case None    => stepFieldValueEmpty
    case Some(v) => stepFieldValueSurround(stepTree(v))
  }
  def stepFieldValueEmpty: X
  def stepFieldValueSurround(value: X): X

  final def stepTree(zipper: StepTreeZipper.DeepZipper): X =
    stepTreeGenSurround(zipper.focus.level,
      zipper.foldMap(step => {
        step.down match {
          case None           => stepTreeNoChildren(step)
          case Some(children) => stepTreeWithChildren(step, children)
        }
      })
    )

  def stepTreeGenSurround(level: Int, gen: X): X

  final def stepLeader(step: StepTreeZipper.AnyFocus): String =
    (if (step.level == 0) step.label else step.node.label).value + "."

  final def stepTreeNoChildren(step: StepTreeZipper.AnyFocus): X =
    stepTreeNoChildren(step, stepLeader(step), stepText(step.value))
  def stepTreeNoChildren(step: StepTreeZipper.AnyFocus, stepLeader: String, text: X): X

  final def stepTreeWithChildren(step: StepTreeZipper.AnyFocus, children: StepTreeZipper.DeepZipper): X =
    stepTreeWithChildren(step, stepLeader(step), stepText(step.value), stepTree(children))
  def stepTreeWithChildren(step: StepTreeZipper.AnyFocus, stepLeader: String, text: X, children: X): X

  final def stepText(value: StepText): X = stepRenderer.step(value)

  // -------------------------------------------------------------------------------------------------------------------
  // Other fields

  def revisionField(f: OF_Revision): X
  def lastUpdatedField(f: OF_LastUpdated): X
  def flowGraphField(f: OF_FlowGraph): X
}
