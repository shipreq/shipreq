package com.beardedlogic.usecase
package lib.field

import model.{FieldSaveCtx, FieldLoadCtx}
import model.FieldValue.FieldValueData

/**
 * Bridge between a field's single state object, and the database.
 *
 * @tparam S Field state type.
 * @since 27/05/2013
 */
trait FieldStateMiniDao[S] {

  /**
   * Sets this object's state to a previously saved state, as provided by the load context.
   *
   * @param ctx A big blob of data for all fields, from which this field should find and use its own data.
   */
  def load(ctx: FieldLoadCtx): S

  /**
   * Gives a field a chance to opt-out of storing a value in the database.
   * If a field is blank, then there's no point saving it.
   */
  def save_?(state: S): Boolean

  /**
   * Saves `Data` and `Value` rows for any additional data required.
   */
  def presave(state: S, ctx: FieldSaveCtx): Unit

  /**
   * Continues saving state to database.
   *
   * Once this is called, the `Data` and `Value` rows for all fields will have been saved, the IDs known.
   *
   * @return A single, arbitrary data string that will be stored in `field_value.data`. The format and mechanism of this
   *         value can be decided by the field type.
   */
  def save(state: S, ctx: FieldSaveCtx): FieldValueData
}
