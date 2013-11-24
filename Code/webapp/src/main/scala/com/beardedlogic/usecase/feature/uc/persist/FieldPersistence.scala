package com.beardedlogic.usecase.feature.uc.persist

import com.beardedlogic.usecase.db.{UcFieldTextWithFK, UseCaseHeader, DaoT}
import com.beardedlogic.usecase.feature.uc.UcParsingCtx
import com.beardedlogic.usecase.feature.uc.field.Field
import com.beardedlogic.usecase.feature.uc.step.StepTree
import com.beardedlogic.usecase.lib.Types._

trait FieldPersistence[F <: Field] {
  type Value = F#Value
  type SavedData

  /**
   * Loads a field value from the database.
   *
   * @param loadCtx A big blob of data for all fields, from which this field should find and use its own data.
   */
  def load(loadCtx: FieldLoadCtx): FieldLoadResult[Value, SavedData]

  def saver(v: Value, stepsAndLabels: StepAndLabelBiMap): FieldSaver[SavedData]

}

// ---------------------------------------------------------------------------------------------------------------------

trait FieldSaver[SavedData] {

  /**
   * Gives a field a chance to opt-out of storing a value in the database.
   * If a field is blank, then there's no point saving it.
   *
   * Note: This is ignored if the field was saved previously. To do otherwise would be to lose audit trail.
   */
  def record_required_? : Boolean

  /**
   * Compares the current field value to the previous saved data.
   *
   * @return Whether the field value has changed.
   */
  def differsFromPrevSave_?(prev: SavedData)(implicit savedSteps: SavedSteps): Boolean

  /**
   * Creates identity rows (`text.id`) for steps.
   *
   * @return A map of new saved steps.
   */
  def presave(dao: DaoT, ucId: UseCaseIdentId, prevSavedSteps: Option[SavedSteps]): Map[LocalStepId, TextIdentId]

  /**
   * Saves field value(s) to the database and links them to the provided UC.
   *
   * @return Data that will be passed back in on subsequent saves to facilitate data reuse (in the DB).
   */
  def save(dao: DaoT, ucId: UseCaseIdentId, ucRevId: UseCaseRevId, prevSave: Option[SavedData])(implicit savedSteps: SavedSteps): SavedData
}

// ---------------------------------------------------------------------------------------------------------------------

case class FieldLoadCtx(header: UseCaseHeader, fieldData: List[UcFieldTextWithFK])

case class FieldLoadResult[+V, +SD](
  savedSteps: Map[LocalStepId, TextIdentId],
  stepTree: Option[StepTree],
  phase2: (SavedSteps, UcParsingCtx) => (V, Option[SD]))

object FieldLoadResult {
  def noSteps[V, SD](phase2: (SavedSteps, UcParsingCtx) => (V, Option[SD])) =
    FieldLoadResult(Map.empty, None, phase2)
}
