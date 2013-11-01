package com.beardedlogic.usecase.feature.publish

import net.liftweb.util.TimeHelpers.logTime
import scalaz.{Need, Traverse, Monoid}
import scalaz.syntax.foldable._
import scalaz.syntax.monoid._

import com.beardedlogic.usecase.feature.uc.UseCase
import com.beardedlogic.usecase.feature.uc.field._
import com.beardedlogic.usecase.feature.uc.step.{StepNode, StepTreeZipper}
import com.beardedlogic.usecase.feature.uc.text.FreeTextTerms._
import com.beardedlogic.usecase.feature.uc.text.{FlowClause, FreeTextTerm, StepText, FreeText}
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
    doc(optionalDocHeader, toc, articles)
  )
  def doc(header: X, toc: X, articles: X): X = header |+| toc |+| articles

  def optionalDocHeader: X = input.header map docHeader getOrElse docHeaderSubst
  def docHeaderSubst: X = zero
  def docHeader(h: DocHeader): X = docHeader(docHeaderTitle(h.title), h.preface.map(docHeaderPreface).getOrElse(zero))
  def docHeader(title: X, preface: X): X
  def docHeaderTitle(t: String): X
  final def docHeaderPreface(p: String) = plainText(p)
  def docHeaderPreface(p: X): X

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
  def fttAnyUseCaseRef(value: AnyUseCaseRef)        : X
  def fttUseCaseRef(value: UseCaseRef)              : X = fttAnyUseCaseRef(value)
  def fttUseCaseSelfRef(value: UseCaseSelfRef)      : X = fttAnyUseCaseRef(value)
  def fttInvalidUseCaseRef(value: InvalidUseCaseRef): X
  def fttMathTex(value: MathTexTerm)                : X

  // -------------------------------------------------------------------------------------------------------------------
  // Step fields

  def stepField(f: OF_Step): X = stepFieldSurround(stepFieldTitle(f.title), stepFieldValue(f))
  def stepFieldSurround(title: X, value: X): X

  final def stepFieldTitle(title: String): X = stepFieldTitleSurround(stepFieldTitleInner(title))
  def stepFieldTitleSurround(title: X): X
  def stepFieldTitleInner(title: String): X = fieldTitle(title)

  final def stepFieldValue(f: OF_Step): X = f.value match {
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

  def revisionField(f: OF_Revision): X
  def lastUpdatedField(f: OF_LastUpdated): X
  def flowGraphField(f: OF_FlowGraph): X
}
