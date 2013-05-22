package com.beardedlogic.usecase.model

import com.beardedlogic.usecase.lib.db.DatabaseEnum

/**
 * Represents types of fields that a use case (or something else) can have.
 *
 * @since 22/05/2013
 */
sealed abstract class FieldKeyType(val ordinal: Short) extends FieldKeyType.Value
object FieldKeyType extends DatabaseEnum[FieldKeyType] {

  override val TableName = "field_key_type"

  /**
   * A field with a name and a single text value.
   */
  case object Text extends FieldKeyType(300)

  val Values = List(Text)
}
