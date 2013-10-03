package com.beardedlogic.usecase
package lib
package field

import db.{DaoT, FieldKeyType, FieldKeyRec, TextRev}
import text.FreeText
import Types._

// =====================================================================================================================

case class TextFieldDefinition(title: String) extends FieldDefinition {
  override val fieldKeyType = FieldKeyType.Text
  override val fieldKeyData = Some(title)
  override def field(rec: FieldKeyRec) = TextField(this, rec)
}

// =====================================================================================================================

case class TextField(override val defn: TextFieldDefinition, override val rec: FieldKeyRec) extends Field {
  override type Value = FreeText
  override type SavedData = TextRev

  override def empty = FreeText.empty

  override def valueSaver(v: FreeText, stepsAndLabels: StepAndLabelBiMap) =
    new TextFieldValueSaver(v, rec, stepsAndLabels)

  override def load(loadCtx: FieldLoadCtx) = {
    val sd = loadCtx.fieldData.find(_.fkId == rec.id).map(_.textRev)
    val text = sd.map(_.text).getOrElse("".hasNormalisedRefs)
    FieldLoadResult.noSteps[Value, SavedData]((savedSteps, stepsAndLabels) => {
      val fv = FreeText.load(text)(savedSteps, stepsAndLabels)
      (fv,sd)
    })
  }

  override def toString = s"${getClass.getSimpleName}[#${rec.id}:${defn.title}]"

  def updateText(newText: String)(uc: UseCase): UcUpdateResult = {
    implicit val lens = alens(Lenses.ucTextFieldL, (uc, this))
    uc.update(this, lens.get.update(newText)(uc.stepsAndLabels))
  }
}

// =====================================================================================================================

class TextFieldValueSaver(val v: FreeText, val fkId: FieldKeyId, val stepsAndLabels: StepAndLabelBiMap) extends FieldValueSaver[TextRev] {
  type SavedData = TextRev

  def textWithNormalisedRefs(implicit savedSteps: SavedSteps) = v.textWithNormalisedRefs(savedSteps)

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
