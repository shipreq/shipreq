package com.beardedlogic.usecase.feature.uc.text

import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.feature.uc.UcParsingCtx
import com.beardedlogic.usecase.feature.uc.change._
import Changes._
import ParsingConfig._
import ParsingUtils._

// =====================================================================================================================

sealed trait FreeTextTerm

object FreeTextTerms {
  case class PlainText(text: String) extends FreeTextTerm

  case object DeletedRef extends FreeTextTerm

  case class StepRef(id: LocalStepId, label: StepLabel) extends FreeTextTerm
  case class InvalidStepRef(label: StepLabel) extends FreeTextTerm

  sealed abstract class AnyUseCaseRef extends FreeTextTerm {
    def num: UseCaseNumber
    def title: String
  }
  case class UseCaseRef(num: UseCaseNumber, title: String) extends AnyUseCaseRef
  case class UseCaseSelfRef(num: UseCaseNumber, title: String) extends AnyUseCaseRef
  case class InvalidUseCaseRef(num: UseCaseNumber, title: Option[String]) extends FreeTextTerm

  case class MathTexTerm(tex: String) extends FreeTextTerm
}

import FreeTextTerms._

// =====================================================================================================================

object FreeText {

  val empty: FreeText = parseCorrected("".tag[InputCorrected])(UcParsingCtx.Empty)

  def correctInput(input: String): String @@ InputCorrected =
    input.trim.tag[InputCorrected]

  def load(text: NormalisedText)(implicit savedSteps: SavedSteps, ctx: UcParsingCtx): FreeText = {
    implicit val stepsAndLabels = ctx.stepsAndLabels
    parseCorrected(realiseNormalisedStepRefs(text))
  }

  def parse(text: String)(implicit ctx: UcParsingCtx): FreeText =
    parseCorrected(correctInput(text))

  def parseCorrected(text: String @@ InputCorrected)(implicit ctx: UcParsingCtx): FreeText = {
    import Grammar.{parse => parseG, _}
    import FreeTextToken._

    lazy val labelsToIds = ctx.getLabelsToIds

    @inline
    def parseTokens(tokens: List[FreeTextToken]): List[FreeTextTerm] =
      tokens map parseToken

    def parseToken(token: FreeTextToken): FreeTextTerm = token match {
      case PlainTextToken(txt)             => PlainText(txt)
      case StepRefToken(true, label)       => parseStepRef(label)
      case StepRefToken(false, label)      => InvalidStepRef(label)
      case UseCaseRefToken(true, num, ot)  => parseUseCaseRef(num, ot)
      case UseCaseRefToken(false, num, ot) => InvalidUseCaseRef(num, ot)
      case DeletedRefToken                 => DeletedRef
      case MathTexToken(inner)             => MathTexTerm(inner)
    }

    @inline
    def parseStepRef(label: StepLabel): FreeTextTerm =
      labelsToIds.get(label) match {
        case Some(id) => StepRef(id, label)
        case None     => InvalidStepRef(label)
      }

    @inline
    def parseUseCaseRef(num: UseCaseNumber, ot: Option[String]): FreeTextTerm =
      if (num == ctx.ucn)
        UseCaseSelfRef(num, ctx.title)
      else
        ctx.rels.findUcTitle(num) match {
          case Some(t) => UseCaseRef(num, t)
          case None    => InvalidUseCaseRef(num, ot)
        }

    parseG(FreeTextParsers.TextAndTokens, text) match {
      case Success(tokens, _) => FreeText(parseTokens(tokens))
      case Failure(_, _)      => FreeText(Nil)
      case e@Error(_, _)      => throw new RuntimeException(s"FreeText parsing error occurred: $e. Text: ${text.inspect}")
    }
  }
}

// =====================================================================================================================

case class FreeText(terms: List[FreeTextTerm]) extends ParsedText {

  val (text, stepRefs, hasUcSelfRef_?) = {
    var stepRefMap = Map.empty[LocalStepId, StepLabel]
    var hasUcSelfRef = false
    val sb = new StringBuilder
    terms.foreach(_ match {
      case PlainText(text)            => sb.append(text)
      case StepRef(id, label)         => sb.appendStepRef(true, label); stepRefMap += (id -> label)
      case InvalidStepRef(label)      => sb.appendStepRef(false, label)
      case DeletedRef                 => sb.append(DeletedRefStr)
      case UseCaseRef(num, title)     => sb.appendUseCaseRef(num, title)
      case UseCaseSelfRef(num, title) => sb.appendUseCaseRef(num, title); hasUcSelfRef = true
      case InvalidUseCaseRef(num, ot) => sb.appendInvalidUseCaseRef(num, ot)
      case MathTexTerm(tex)           => sb.appendMathTexTerm(tex)
    })
    (sb.toString, stepRefMap, hasUcSelfRef)
  }

  def hasStepRefs_? = stepRefs.nonEmpty

  override def normalisedText(implicit savedSteps: SavedSteps) = normalise(text, stepRefs, savedSteps)
}

// =====================================================================================================================

class FreeTextUpdater(textChanged: Change) extends ParsedTextUpdater[FreeText] with SeqChangeResponder[FreeText] {

  override def correctInput(input: String) = FreeText.correctInput(input)

  override def updateCorrected(t: FreeText, newText: String @@ InputCorrected)(implicit ctx: UcParsingCtx) =
    FreeText.parseCorrected(newText) @: textChanged

  override def respondToChange(t: FreeText, c: Change)(implicit  ctx: UcParsingCtx) =
    c match {
      case _: ExistingStepLabelsChanged                  => updateAfterStepTreeChange(t)
      case TitleChanged(_, newTitle) if t.hasUcSelfRef_? => updateUcSelfRefs(t, newTitle)
      case _                                             => NoChange
    }

  /**
   * Updates references when the step tree structure changes.
   */
  def updateAfterStepTreeChange(t: FreeText)(implicit ctx: UcParsingCtx): ChangeResult[FreeText, Change] =
    if (!t.hasStepRefs_?)
      NoChange
    else {
      import FreeTextTerms._
      lazy val idsToLabels = ctx.getIdsToLabels

      var changed = false
      val newTerms: List[FreeTextTerm] =
        t.terms.map(unchanged => unchanged match {
          case StepRef(id, oldLabel) =>
            idsToLabels.get(id) match {
              case None                                   => changed = true; DeletedRef
              case Some(newLabel) if oldLabel != newLabel => changed = true; StepRef(id, newLabel)
              case _                                      => unchanged
            }

          case PlainText(_)
               | InvalidStepRef(_)
               | DeletedRef
               | MathTexTerm(_)
               | UseCaseRef(_, _)
               | UseCaseSelfRef(_, _)
               | InvalidUseCaseRef(_, _) => unchanged
        })

      if (changed)
        t.copy(terms = newTerms) @: textChanged
      else
        NoChange
    }

  def updateUcSelfRefs(t: FreeText, newTitle: String): ChangeResult[FreeText, Change] = {
    val newTerms = t.terms.map(_ match {
      case UseCaseSelfRef(num, _) => UseCaseSelfRef(num, newTitle)
      case term                   => term
    })
    t.copy(terms = newTerms) @: textChanged
  }
}