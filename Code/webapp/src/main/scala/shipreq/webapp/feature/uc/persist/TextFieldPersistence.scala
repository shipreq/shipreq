package shipreq.webapp.feature.uc
package persist

import shipreq.webapp.db.{DaoT, TextRev}
import shipreq.webapp.lib.Types._
import field.TextField
import text.FreeText

case class TextFieldPersistence(f: TextField) extends FieldPersistence[TextField] {
  override type SavedData = TextRev

  import f.rec

  override def saver(v: FreeText, stepsAndLabels: StepAndLabelBiMap) =
    new TextFieldSaver(v, rec, stepsAndLabels)

  override def load(loadCtx: FieldLoadCtx) = {
    val sd = loadCtx.fieldData.find(_.fkId == rec.id).map(_.textRev)
    val text = sd.fold(NormalisedText(""))(_.text)
    FieldLoadResult.noSteps[Value, SavedData]((savedSteps, stepsAndLabels) => {
      val fv = FreeText.load(text)(savedSteps, stepsAndLabels)
      (fv,sd)
    })
  }
}

// =====================================================================================================================

class TextFieldSaver(val v: FreeText, val fkId: FieldKeyId, val stepsAndLabels: StepAndLabelBiMap) extends FieldSaver[TextRev] {
  type SavedData = TextRev

  def textWithNormalisedRefs(implicit savedSteps: SavedSteps) = v.normalisedText(savedSteps)

  override def record_required_? = v.text.nonEmpty

  override def differsFromPrevSave_?(prev: SavedData)(implicit savedSteps: SavedSteps): Boolean =
    textWithNormalisedRefs != prev.text

  override def presave(dao: DaoT, ucId: UseCaseIdentId, prevSavedSteps: Option[SavedSteps]) = Map.empty

  override def save(dao: DaoT, ucId: UseCaseIdentId, ucRevId: UseCaseRevId, prevSave: Option[SavedData])(implicit savedSteps: SavedSteps): SavedData = {
    val curText = textWithNormalisedRefs

    val textRev = prevSave match {
      // Reuse
      case Some(prev) if prev.text == curText => prev
      // Update step
      case Some(prev) => dao.createTextRev(prev.identId, (prev.rev + 1).toShort, curText)
      // New step
      case None =>
        val textIdentId = dao.createTextIdent(ucId, fkId)
        dao.createTextRev(textIdentId, 1: Short, curText)
    }

    dao.linkUcToText(ucRevId, textRev)

    textRev
  }
}
