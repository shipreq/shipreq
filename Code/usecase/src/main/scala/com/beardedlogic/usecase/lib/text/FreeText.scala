package com.beardedlogic.usecase.lib.text

import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.lib.change._
import Changes._
import ParsingConfig._
import ParsingUtils._

object FreeText extends Parser[FreeText] {

  override val empty: FreeText = parseCorrected("")(EmptyStepAndLabelBiMap)

  def correctInput(input: String) = input.trim

  override def load(text: TextWithNormalisedRefs)(implicit savedSteps: SavedSteps, stepsAndLabels: StepAndLabelBiMap) =
    parseCorrected(realiseNormalisedRefs(text))

  override def parse(text: String)(implicit stepsAndLabels: StepAndLabelBiMap) = parseCorrected(correctInput(text))

  /**
   * Parses plain text and does the following:
   *
   * 1) Records a map of ids-to-labels, of valid references.
   * 2) Removes whitespace from references.
   * 3) Appends a ? to invalid references.
   */
  def parseCorrected(text: String)(implicit stepsAndLabels: StepAndLabelBiMap) = {
    lazy val labelsToIds = stepsAndLabels.get.ba

    val newText = new StringBuilder
    var refs = Map.empty[LocalIdStr, LabelStr]

    // Parse input
    var r = Grammar.parse(Grammar.TextAndPossibleRef, text)
    while (r.get._2.isDefined) {
      newText ++= r.get._1

      // Check label validity
      val label = r.get._2.get.asLabel
      val id = labelsToIds.get(label)
      if (id.isDefined) {
        if (!refs.contains(id.get)) refs += (id.get -> label)
        makeRef(newText, label)
      } else
        makeInvalidRef(newText, label)

      // Continue parsing
      r = Grammar.parse(Grammar.TextAndPossibleRef, r.next)
    }
    newText ++= r.get._1

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
case class FreeText(text: String, refs: Map[LocalIdStr, LabelStr]) extends ParsedText[FreeText] {

  override def textWithNormalisedRefs(implicit savedSteps: SavedSteps) = normaliseRefs(text, refs, savedSteps)

  override def hasRefs_? = refs.nonEmpty

  override protected def correctInput(input: String) = FreeText.correctInput(input)

  protected def textChanged = TextChanged

  override protected def updateCorrected(newText: String)(implicit stepsAndLabels: StepAndLabelBiMap) = {
    FreeText.parseCorrected(newText) @: textChanged
  }

  override def respondToChange(c: Change)(implicit stepsAndLabels: StepAndLabelBiMap) = c match {
    case _: ExistingStepLabelsChanged => updateRefs // Update step references when they change
    case _ => NoChange
  }

  /** Updates `refs` and creates a copy of `text` in which all references are up-to-date. */
  def updateRefs(implicit stepsAndLabels: StepAndLabelBiMap): ChangeResult[FreeText, Change] = {
    if (!hasRefs_?) NoChange
    else migrateRefsToNewStepTree(this) match {
      case Some(updated) => updated @: textChanged
      case _ => NoChange
    }
  }
}
