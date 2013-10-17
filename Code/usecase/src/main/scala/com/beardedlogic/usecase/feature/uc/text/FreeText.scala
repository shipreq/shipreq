package com.beardedlogic.usecase.feature.uc.text

import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.feature.uc.UcParsingCtx
import com.beardedlogic.usecase.feature.uc.change._
import Changes._
import ParsingConfig._
import ParsingUtils._

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

    val newText = new StringBuilder
    var refs = Map.empty[LocalStepId, StepLabel]
    var refsOwnUc = false

    @inline
    def parseToken(ref: FreeTextToken): Unit = ref match {
      case PlainTextToken(txt)      => newText.append(txt)
      case StepLabelRefToken(label) => appendStepRef(label)
      case UseCaseRefToken(num, ot) => appendUseCaseRef(num, ot)
    }

    @inline
    def appendStepRef(label: StepLabel): Unit =
      labelsToIds.get(label) match {
        case Some(id) =>
          refs += (id -> label)
          newText.appendStepRef(true, label)
        case None =>
          newText.appendStepRef(false, label)
      }

    @inline
    def appendUseCaseRef(num: UseCaseNumber, ot: Option[String]): Unit = {
      if (num == ctx.ucn) {
        refsOwnUc = true
        newText.appendUseCaseRef(num, ctx.title)
      } else
        ctx.rels.findUcTitle(num) match {
          case Some(t) => newText.appendUseCaseRef(num, t)
          case None    => newText.appendInvalidUseCaseRef(num, ot)
        }
    }

    parseG(FreeTextParsers.TextAndRefs, text) match {
      case Success(terms, _) => terms foreach parseToken
      case pr @ NoSuccess(_, _) =>
        throw new RuntimeException(s"FreeText parsing failure shouldn't be possible. Got: $pr. Text: ${text.inspect}")
    }

    FreeText(newText.toString, refs, refsOwnUc)
  }
}

// ---------------------------------------------------------------------------------------------------------------------

/**
 * Encapsulates a String to provide the following functionality:
 *
 * 1) References to steps...
 * - are validated and cleaned up (ie. whitespace in braces removed).
 * - invalid references are transformed to make the invalidity obvious.
 * - are updated when their labels change.
 *
 * @since 12/05/2013 (as SmartText)
 * @since 16/07/2013 (as FreeText)
 */
case class FreeText(text: String, refs: Map[LocalStepId, StepLabel], refsOwnUc: Boolean) extends ParsedText[FreeText] {

  override def normalisedText(implicit savedSteps: SavedSteps) = normalise(text, refs, savedSteps)

  override def hasRefs_? = refs.nonEmpty

  override protected def correctInput(input: String) = FreeText.correctInput(input)

  protected def textChanged = TextChanged

  override protected def updateCorrected(newText: String @@ InputCorrected)(implicit ctx: UcParsingCtx) = {
    FreeText.parseCorrected(newText) @: textChanged
  }

  override def respondToChange(c: Change)(implicit ctx: UcParsingCtx) = c match {
    case _: ExistingStepLabelsChanged             => updateStepRefs
    case TitleChanged(before, after) if refsOwnUc => updateUcRefs(before, after)
    case _                                        => NoChange
  }

  /** Updates `refs` and creates a copy of `text` in which all references are up-to-date. */
  def updateStepRefs(implicit ctx: UcParsingCtx): ChangeResult[FreeText, Change] = {
    if (!hasRefs_?) NoChange
    else migrateRefsToNewStepTree(this)(ctx.stepsAndLabels) match {
      case Some(updated) => updated @: textChanged
      case _ => NoChange
    }
  }

  def updateUcRefs(before: String, after: String)(implicit ctx: UcParsingCtx): ChangeResult[FreeText, Change] = {
    val oldRef = makeUseCaseRef(ctx.ucn, before)
    val newRef = makeUseCaseRef(ctx.ucn, after)
    val r = copy(text.replace(oldRef, newRef))
    r @: textChanged
  }
}
