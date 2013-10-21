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
}

import FreeTextTerms._

// =====================================================================================================================

object FreeText extends Parser[FreeText] {

  override val empty: FreeText = parseCorrected("".tag[InputCorrected])(UcParsingCtx.Empty)

  def correctInput(input: String): String @@ InputCorrected = input.trim.tag[InputCorrected]

  override def load(text: NormalisedText)(implicit savedSteps: SavedSteps, ctx: UcParsingCtx) = {
    implicit val stepsAndLabels = ctx.stepsAndLabels
    parseCorrected(realiseNormalisedStepRefs(text))
  }

  override def parse(text: String)(implicit ctx: UcParsingCtx) =
    parseCorrected(correctInput(text))

  def parseCorrected(text: String @@ InputCorrected)(implicit ctx: UcParsingCtx) = {
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

    parseG(FreeTextParsers.TextAndRefs, text) match {
      case Success(tokens, _) => FreeText(parseTokens(tokens))
      case Failure(_, _)      => FreeText(Nil)
      case e@Error(_, _)      => throw new RuntimeException(s"FreeText parsing error occurred: $e. Text: ${text.inspect}")
    }
  }
}

// =====================================================================================================================

/**
 * @since 12/05/2013 (as SmartText)
 * @since 16/07/2013 (as FreeText)
 */
case class FreeText(terms: List[FreeTextTerm]) extends ParsedText[FreeText] {

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
    })
    (sb.toString, stepRefMap, hasUcSelfRef)
  }

  def hasStepRefs_? = stepRefs.nonEmpty

  override def normalisedText(implicit savedSteps: SavedSteps) = normalise(text, stepRefs, savedSteps)

  override protected def correctInput(input: String) = FreeText.correctInput(input)

  protected def textChanged = TextChanged

  override protected def updateCorrected(newText: String @@ InputCorrected)(implicit ctx: UcParsingCtx) = {
    FreeText.parseCorrected(newText) @: textChanged
  }

  override def respondToChange(c: Change)(implicit ctx: UcParsingCtx) = c match {
    case _: ExistingStepLabelsChanged                => updateAfterStepTreeChange
    case TitleChanged(_, newTitle) if hasUcSelfRef_? => updateUcSelfRefs(newTitle)
    case _                                           => NoChange
  }

  /**
   * Updates references when the step tree structure changes.
   */
  def updateAfterStepTreeChange(implicit ctx: UcParsingCtx): ChangeResult[FreeText, Change] =
    if (!hasStepRefs_?)
      NoChange
    else {
      import FreeTextTerms._
      lazy val idsToLabels = ctx.getIdsToLabels

      var changed = false
      val newTerms: List[FreeTextTerm] =
        terms.map(unchanged => unchanged match {
          case StepRef(id, oldLabel) =>
            idsToLabels.get(id) match {
              case None                                   => changed = true; DeletedRef
              case Some(newLabel) if oldLabel != newLabel => changed = true; StepRef(id, newLabel)
              case _                                      => unchanged
            }

          case PlainText(_)
               | InvalidStepRef(_)
               | DeletedRef
               | UseCaseRef(_, _)
               | UseCaseSelfRef(_, _)
               | InvalidUseCaseRef(_, _) => unchanged
        })

      if (changed) FreeText(newTerms) @: textChanged
      else NoChange
    }

  def updateUcSelfRefs(newTitle: String): ChangeResult[FreeText, Change] = {
    val newTerms = terms.map(t => t match {
      case UseCaseSelfRef(num, _) => UseCaseSelfRef(num, newTitle)
      case _ => t
    })
    FreeText(newTerms) @: textChanged
  }
}
