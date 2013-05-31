package com.beardedlogic.usecase
package lib.field

import model.{MutableFieldSaveCtx, FieldLoadCtx}

/**
 * Loads a field's state to the database.
 *
 * @tparam S Field state type.
 * @since 27/05/2013
 */
trait FieldStateLoader[S] {

  /**
   * Sets this object's state to a previously saved state, as provided by the load context.
   *
   * @param loadCtx A big blob of data for all fields, from which this field should find and use its own data.
   * @param saveCtx After loading, a load ctx is transformed into a save ctx so that it can be used as a save
   *                checkpoint. Fields should update the saveCtx as required as they process the load ctx.
   */
  def load(loadCtx: FieldLoadCtx, saveCtx: MutableFieldSaveCtx): S
}
