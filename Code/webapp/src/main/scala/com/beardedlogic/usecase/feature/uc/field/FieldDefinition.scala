package com.beardedlogic.usecase
package feature.uc.field

import db.{FieldKeyType, FieldKeyRec}
import lib.Types.FieldKeyRecData

trait FieldDefinition {

  /** The type (enum) of this field. */
  val fieldKeyType: FieldKeyType

  /** Arbitrary data (to store in the database) that comprises this field key's state. */
  val fieldKeyData: FieldKeyRecData

  def field(rec: FieldKeyRec): Field
}
