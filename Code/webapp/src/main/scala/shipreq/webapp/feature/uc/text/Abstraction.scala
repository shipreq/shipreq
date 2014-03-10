package shipreq.webapp.feature.uc.text

import shipreq.webapp.lib.Types._
import shipreq.webapp.feature.uc.change._
import shipreq.webapp.feature.uc.UcParsingCtx

trait ParsedText {
  val text: String
  @inline final def isEmpty = text.isEmpty
  @inline final def nonEmpty = text.nonEmpty
  def normalisedText(implicit savedSteps: SavedSteps): NormalisedText
}

trait ParsedTextUpdater[T <: ParsedText] {
  def correctInput(input: String): String @@ InputCorrected

  final def update(t: T, input: String)(implicit ctx: UcParsingCtx): ChangeResult[T, Change] =
    updateCorrected(t, correctInput(input))

  final def updateCorrected(t: T, newText: String @@ InputCorrected)(implicit ctx: UcParsingCtx): ChangeResult[T, Change] = {
    if (t.text == newText)
      NoChange
    else
      updateCorrected2(t, newText)
  }

  protected def updateCorrected2(t: T, newText: String @@ InputCorrected)(implicit ctx: UcParsingCtx): ChangeResult[T, Change]

  final def updateAndGet(t: T, input: String)(implicit ctx: UcParsingCtx): T = update(t, input).getValueOrElse(t)
  final def updateCorrectedAndGet(t: T, input: String @@ InputCorrected)(implicit ctx: UcParsingCtx): T = updateCorrected(t, input).getValueOrElse(t)

}