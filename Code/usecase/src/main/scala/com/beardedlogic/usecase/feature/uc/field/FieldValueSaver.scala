package com.beardedlogic.usecase
package feature.uc.field

import db.DaoT
import lib.Types._

trait FieldValueSaver[SavedData] {

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
