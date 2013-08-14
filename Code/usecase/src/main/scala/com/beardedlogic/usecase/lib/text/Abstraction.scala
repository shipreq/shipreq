package com.beardedlogic.usecase.lib.text

import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.lib.change._

trait Parser[T <: ParsedText[T]] {

  def empty: T

  def load(text: TextWithNormalisedRefs)(implicit savedSteps: SavedSteps, stepsAndLabels: StepAndLabelBiMap): T

  def parse(text: String)(implicit stepsAndLabels: StepAndLabelBiMap): T
}

trait ParsedText[Self <: ParsedText[Self]] extends ChangeResponder[Self] {
  this: Self =>

  val text: String

  @inline final def isEmpty = text.isEmpty
  @inline final def nonEmpty = text.nonEmpty

  def textWithNormalisedRefs(implicit savedSteps: SavedSteps): TextWithNormalisedRefs

  //  def refs: Refs

  /** Does this text contain any step references? */
  def hasRefs_? : Boolean

  def update(input: String)(implicit stepsAndLabels: StepAndLabelBiMap): ChangeResult[Self, Change] = {
    val newText = correctInput(input)
    if (text == newText) NoChange
    else updateCorrected(newText)
  }

  protected def correctInput(input: String): String

  protected def updateCorrected(newText: String)(implicit stepsAndLabels: StepAndLabelBiMap): ChangeResult[Self, Change]
}
