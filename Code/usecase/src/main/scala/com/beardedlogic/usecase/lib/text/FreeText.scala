package com.beardedlogic.usecase.lib.text

import scala.annotation.tailrec
import com.beardedlogic.usecase.lib.UcParsingCtx
import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.lib.change._
import Changes._
import ParsingConfig._
import ParsingUtils._

object FreeText extends Parser[FreeText] {

  override val empty: FreeText = parseCorrected("")(UcParsingCtx.Empty)

  def correctInput(input: String) = input.trim

  override def load(text: TextWithNormalisedRefs)(implicit savedSteps: SavedSteps, ctx: UcParsingCtx) = {
    implicit val stepsAndLabels = ctx.stepsAndLabels
    parseCorrected(realiseNormalisedRefs(text))
  }

  override def parse(text: String)(implicit ctx: UcParsingCtx) =
    parseCorrected(correctInput(text))

  /**
   * Parses plain text and does the following:
   *
   * 1) Records a map of ids-to-labels, of valid references.
   * 2) Removes whitespace from references.
   * 3) Appends a ? to invalid references.
   */
  def parseCorrected(text: String)(implicit ctx: UcParsingCtx) = {
    import Grammar.{parse => parseG, _}
    lazy val labelsToIds = ctx.getLabelsToIds

    val newText = new StringBuilder
    var refs = Map.empty[LocalStepId, StepLabel]

    @tailrec
    def go(pr: ParseResult[(String, Option[FreeTextRefToken])]): Unit = {
      pr match {
        case Success((txt, None), _) =>
          newText ++= txt
        case Success((txt, Some(ref)), next) =>
          newText ++= txt
          parseRef(ref)
          go(parseG(TextAndPossibleRef, next))
        case NoSuccess(_, _) =>
          error(s"TextAndPossibleRef failure shouldn't be possible. Got: $pr. Text: ${text.inspect}")
      }
    }

    def parseRef(ref: FreeTextRefToken): Unit = ref match {
      case StepLabelRefToken(label) => parseStepLabel(label)
    }

    def parseStepLabel(label: StepLabel): Unit = {
      val id = labelsToIds.get(label)
      if (id.isDefined) {
        refs += (id.get -> label)
        newText.appendRef(label)
      } else
        newText.appendInvalidRef(label)
    }

    go(parseG(TextAndPossibleRef, text))
    FreeText(newText.toString, refs)
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
case class FreeText(text: String, refs: Map[LocalStepId, StepLabel]) extends ParsedText[FreeText] {

  override def textWithNormalisedRefs(implicit savedSteps: SavedSteps) = normaliseRefs(text, refs, savedSteps)

  override def hasRefs_? = refs.nonEmpty

  override protected def correctInput(input: String) = FreeText.correctInput(input)

  protected def textChanged = TextChanged

  override protected def updateCorrected(newText: String)(implicit ctx: UcParsingCtx) = {
    FreeText.parseCorrected(newText) @: textChanged
  }

  override def respondToChange(c: Change)(implicit ctx: UcParsingCtx) = c match {
    case _: ExistingStepLabelsChanged => updateRefs // Update step references when they change
    case _ => NoChange
  }

  /** Updates `refs` and creates a copy of `text` in which all references are up-to-date. */
  def updateRefs(implicit ctx: UcParsingCtx): ChangeResult[FreeText, Change] = {
    if (!hasRefs_?) NoChange
    else migrateRefsToNewStepTree(this)(ctx.stepsAndLabels) match {
      case Some(updated) => updated @: textChanged
      case _ => NoChange
    }
  }
}
