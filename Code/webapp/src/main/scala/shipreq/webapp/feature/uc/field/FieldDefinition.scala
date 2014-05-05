package shipreq.webapp
package feature.uc.field

import shipreq.webapp.db.{FieldKeyRecData, FieldKeyType, FieldKeyRec}

trait FieldDefinition {

  /** The type (enum) of this field. */
  val fieldKeyType: FieldKeyType

  /** Arbitrary data (to store in the database) that comprises this field key's state. */
  val fieldKeyData: FieldKeyRecData

  def field(rec: FieldKeyRec): Field
}
