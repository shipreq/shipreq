package com.beardedlogic.usecase.feature.uc.text

import com.beardedlogic.usecase.lib.Types._
import com.beardedlogic.usecase.feature.uc.change._
import com.beardedlogic.usecase.feature.uc.UcParsingCtx

trait ParsedText {
  val text: String
  @inline final def isEmpty = text.isEmpty
  @inline final def nonEmpty = text.nonEmpty
  def normalisedText(implicit savedSteps: SavedSteps): NormalisedText
}

trait ParsedTextUpdater[T <: ParsedText] extends ChangeResponder[T] {
  def t: T
  def correctInput(input: String): String @@ InputCorrected
  def updateCorrected(newText: String @@ InputCorrected)(implicit ctx: UcParsingCtx): ChangeResult[T, Change]

  final def update(input: String)(implicit ctx: UcParsingCtx): ChangeResult[T, Change] = {
    val newText = correctInput(input)
    if (t.text == newText)
      NoChange
    else
      updateCorrected(newText)
  }
}