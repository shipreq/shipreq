package com.beardedlogic.shipreq
package feature.publish

import scalaz.Monoid
import feature.uc.text.{FlowClause, StepText, FreeTextTerm}
import feature.uc.text.FreeTextTerms._
import lib.ScalazSubset._
import lib.Types._

object Helpers {

  def markupAndRenderFTTs[X](terms: List[FreeTextTerm])(implicit markupTokenRenderer: MarkupTokenRenderer[X]): X =
    markupTokenRenderer.markupTokens(
      TextMarkup.markup(
        TextMarkup.introduce(
          terms)))
}

// =====================================================================================================================

trait MarkupTokenRenderer[X] {
  def markupTokens(tokens: List[MarkupToken]): X
}

abstract class MarkupTokenAndGapRenderer[X](implicit xMonoid: Monoid[X]) extends MarkupTokenRenderer[X] {
  override final def markupTokens(tokens: List[MarkupToken]) = {
    var prev: MarkupToken = null
    tokens.foldLeft(xMonoid.zero)((acc, mt) => {
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
}

// =====================================================================================================================

trait FTTRenderer[X] {
  final def term(term: FreeTextTerm): X = term match {
    case t: PlainText         => fttPlainText(t)
    case t: StepRef           => fttStepRef(t)
    case t: InvalidStepRef    => fttInvalidStepRef(t)
    case    DeletedRef        => fttDeletedRef
    case t: UseCaseRef        => fttUseCaseRef(t)
    case t: UseCaseSelfRef    => fttUseCaseSelfRef(t)
    case t: InvalidUseCaseRef => fttInvalidUseCaseRef(t)
    case t: MathTexTerm       => fttMathTex(t)
  }
  def fttPlainText(value: PlainText)                : X
  def fttDeletedRef                                 : X
  def fttStepRef(value: StepRef)                    : X
  def fttInvalidStepRef(value: InvalidStepRef)      : X
  def fttUseCaseRef(value: UseCaseRef)              : X
  def fttUseCaseSelfRef(value: UseCaseSelfRef)      : X
  def fttInvalidUseCaseRef(value: InvalidUseCaseRef): X
  def fttMathTex(value: MathTexTerm)                : X
}

trait SeparateUcRefsByScope[X] {
  this: FTTRenderer[X] =>
  def inScope(num: UseCaseNumber): Boolean
  final override def fttUseCaseRef(r: UseCaseRef) =
    if (inScope(r.num))
      fttUseCaseRefInScope(r)
    else
      fttUseCaseRefOutOfScope(r)
  def fttUseCaseSelfRef(value: UseCaseSelfRef)  : X = fttUseCaseRefInScope(value)
  def fttUseCaseRefOutOfScope(value: UseCaseRef): X
  def fttUseCaseRefInScope(value: AnyUseCaseRef): X
}


trait SeparateUcRefsByScopeAndUseLinkRenderer[X] extends SeparateUcRefsByScope[X] {
  this: FTTRenderer[X] =>
  def linkRenderer: LinkRenderer[X]
  override final def fttStepRef(value: StepRef)                 = linkRenderer.stepRef(value.label)
  override final def fttUseCaseRefInScope(value: AnyUseCaseRef) = linkRenderer.usecaseRef(value.num, value.title)
}


trait UseLinkRendererInFTTs[X] {
  this: FTTRenderer[X] =>
  def linkRenderer: LinkRenderer[X]
  override final def fttStepRef(value: StepRef)               = linkRenderer.stepRef(value.label)
  override final def fttUseCaseRef(value: UseCaseRef)         = linkRenderer.usecaseRef(value.num, value.title)
  override final def fttUseCaseSelfRef(value: UseCaseSelfRef) = linkRenderer.usecaseRef(value.num, value.title)
}

// =====================================================================================================================

trait LinkRenderer[X] {
  def stepRef(l: StepLabel): X
  def usecaseRef(num: UseCaseNumber, title: String): X
  final def usecaseRef(r: AnyUseCaseRef): X = usecaseRef(r.num, r.title)
}

// =====================================================================================================================

trait StepRenderer[X] {
  def step(value: StepText): X
}

trait StepFlowRenderer[X] {
  def flowClause(arrow: String, refs: X): X
  def flowRefSep: X
}

class TypicalStepRenderer[X](lr: LinkRenderer[X], fr: StepFlowRenderer[X])(implicit xm: Monoid[X], mtr: MarkupTokenRenderer[X])
  extends StepRenderer[X] {

  def step(value: StepText): X =
    Helpers.markupAndRenderFTTs(value.mainClause.terms) |+| flowClause(value.flowFromClause) |+| flowClause(value.flowToClause)

  def flowClause(oc: Option[FlowClause]): X =
    oc match {
      case None    => xm.zero
      case Some(c) => flowClause(c)
    }

  def flowClause(c: FlowClause): X =
    fr.flowClause(c.flowObj.style.arrow, c.sortedLabels.toList map flowRef intercalate fr.flowRefSep)

  def flowRef(l: StepLabel): X =
    lr.stepRef(l)
}