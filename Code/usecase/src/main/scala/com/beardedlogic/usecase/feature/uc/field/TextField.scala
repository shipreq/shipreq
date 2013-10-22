package com.beardedlogic.usecase
package feature.uc
package field

import db.{DaoT, FieldKeyType, FieldKeyRec, TextRev}
import lib.Types._
import change.UseCaseUpdater
import change.Changes.TextChanged
import text.{FreeTextUpdater, FreeText}

// =====================================================================================================================

case class TextFieldDefinition(title: String) extends FieldDefinition {
  override val fieldKeyType = FieldKeyType.Text
  override val fieldKeyData = Some(title)
  override def field(rec: FieldKeyRec) = TextField(this, rec)
}

// =====================================================================================================================

trait TextFieldLike { this: Field with TextField =>
  override type Value = FreeText
  override type SavedData = TextRev

  override def empty = FreeText.empty

  override def valueSaver(v: FreeText, stepsAndLabels: StepAndLabelBiMap) =
    new TextFieldValueSaver(v, rec, stepsAndLabels)

  override def load(loadCtx: FieldLoadCtx) = {
    val sd = loadCtx.fieldData.find(_.fkId == rec.id).map(_.textRev)
    val text = sd.map(_.text).getOrElse("".tag[IsNormalised])
    FieldLoadResult.noSteps[Value, SavedData]((savedSteps, stepsAndLabels) => {
      val fv = FreeText.load(text)(savedSteps, stepsAndLabels)
      (fv,sd)
    })
  }

  override def toString = s"${getClass.getSimpleName}[#${rec.id}:${defn.title}]"

  override def changeResponder(v: FreeText) = FreeTextUpdater(v, textChanged)

  def updateText(newText: String)(u: UseCaseUpdater): UcUpdateResult = {
    implicit val lens = alens(Lenses.ucTextFieldL, (u.uc, this))
    val updater = FreeTextUpdater(lens.get, textChanged)
    val cr = updater.update(newText)(u.ctx)
    u.update(this, cr)
  }

  private val textChanged = TextChanged(this)
}

// =====================================================================================================================

class TextFieldValueSaver(val v: FreeText, val fkId: FieldKeyId, val stepsAndLabels: StepAndLabelBiMap) extends FieldValueSaver[TextRev] {
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
