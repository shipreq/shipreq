package com.beardedlogic.usecase.feature.publish

import net.liftweb.util.TimeHelpers.logTime
import scalaz.{Need, Traverse, Monoid}

import com.beardedlogic.usecase.feature.uc.UseCase
import com.beardedlogic.usecase.feature.uc.field._
import com.beardedlogic.usecase.feature.uc.step.{StepNode, StepTreeZipper}
import com.beardedlogic.usecase.feature.uc.text.FreeTextTerms._
import com.beardedlogic.usecase.feature.uc.text.{FlowClause, FreeTextTerm, StepText, FreeText}
import com.beardedlogic.usecase.lib.ScalazSubset._
import com.beardedlogic.usecase.lib.Misc.DateTimeExt
import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.feature.FlowGraph
import com.beardedlogic.usecase.db.UseCaseRev

abstract class GenericPublisher(input: Input) {

  type X
  implicit def xMonoid: Monoid[X]
  implicit def listMonoid[A]: Monoid[List[A]] = scalaz.std.list.listMonoid
  implicit def listInstance: Traverse[List] = scalaz.std.list.listInstance

  @inline final def zero = xMonoid.zero

  @inline final def useCases = input.sortedUseCases

  @inline final def inScope(num: UseCaseNumber) = useCases.exists(_.number == num)

  final def plainText(value: String): X =
    plainText(List(PlainText(value)))

  final def plainText(terms: List[FreeTextTerm]): X =
    markupTokens(
      TextMarkup.markup(
        TextMarkup.introduce(
          terms)))

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

  def optionalDocHeader: X = input.header map docHeader getOrElse docHeaderSubst
  def docHeaderSubst: X = zero
  def docHeader(h: DocHeader): X = docHeader(docHeaderTitle(h.title), h.preface.map(docHeaderPreface).getOrElse(zero))
  def docHeader(title: X, preface: X): X
  def docHeaderTitle(t: String): X
  final def docHeaderPreface(p: String) = plainText(p)
  def docHeaderPreface(p: X): X

  def optionalDocLastUpdated: X = input.lastUpdated.map(t => docLastUpdated(t.toIso8601Str)) getOrElse zero
  def docLastUpdated(t: String @@ ISO8601): X

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

  final def text(value: FreeText): X = plainText(value.terms)

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
    case t: PlainText         => fttPlainText(t)
    case t: StepRef           => fttStepRef(t)
    case t: InvalidStepRef    => fttInvalidStepRef(t)
    case t@ DeletedRef        => fttDeletedRef
    case t: UseCaseRef        => fttUseCaseRef(t)
    case t: UseCaseSelfRef    => fttUseCaseSelfRef(t)
    case t: InvalidUseCaseRef => fttInvalidUseCaseRef(t)
    case t: MathTexTerm       => fttMathTex(t)
  }

  def fttPlainText(value: PlainText)                : X
  def fttDeletedRef                                 : X
  def fttStepRef(value: StepRef)                    : X
  def fttInvalidStepRef(value: InvalidStepRef)      : X
  def fttUseCaseRefOutOfScope(value: UseCaseRef)    : X
  def fttUseCaseRefInScope(value: AnyUseCaseRef)    : X
  def fttUseCaseSelfRef(value: UseCaseSelfRef)      : X = fttUseCaseRefInScope(value)
  def fttInvalidUseCaseRef(value: InvalidUseCaseRef): X
  def fttMathTex(value: MathTexTerm)                : X

  final def fttUseCaseRef(r: UseCaseRef) =
    if (inScope(r.num)) fttUseCaseRefInScope(r)
    else                fttUseCaseRefOutOfScope(r)

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
    (if (step.level == 0) step.label else step.node.label) + "."

  final def stepTreeNoChildren(step: StepTreeZipper.AnyFocus): X =
    stepTreeNoChildren(step, stepLeader(step), stepText(step.value))
  def stepTreeNoChildren(step: StepTreeZipper.AnyFocus, stepLeader: String, text: X): X

  final def stepTreeWithChildren(step: StepTreeZipper.AnyFocus, children: StepTreeZipper.DeepZipper): X =
    stepTreeWithChildren(step, stepLeader(step), stepText(step.value), stepTree(children))
  def stepTreeWithChildren(step: StepTreeZipper.AnyFocus, stepLeader: String, text: X, children: X): X

  def stepText(value: StepText): X =
    text(value.mainClause) |+| flowClause(value.flowFromClause) |+| flowClause(value.flowToClause)

  def flowClause(oc: Option[FlowClause]): X =
    oc match {
      case None => zero
      case Some(c) => flowClause(c)
    }
  def flowClause(c: FlowClause): X =
    flowClause(c.flowObj.style.arrow, c.sortedLabels.toList map flowRef intercalate flowRefSep)

  def flowClause(arrow: String, refs: X): X
  def flowRef(l: StepLabel): X
  def flowRefSep: X

  // -------------------------------------------------------------------------------------------------------------------
  // Other fields

  def revisionField(f: OF_Revision): X
  def lastUpdatedField(f: OF_LastUpdated): X
  def flowGraphField(f: OF_FlowGraph): X
}
