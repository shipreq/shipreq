package com.beardedlogic.usecase
package model

import lib.field.FieldDefinition

case class FieldListRec(fieldKeys: List[FieldKeyRec]) {
  val fields = fieldKeys.map(_.field)
  def fieldDefns = fieldKeys.map(_.fieldDefn)
}

trait FieldListAccessor extends DatabaseAccessor {
  self: FieldKeyAccessor =>

  /**
   * Ensures that a data & value exist in the DB that matches the given field list, and that it is the latest revision.
   *
   * @param fields The field list to save.
   */
  def syncFieldList(fields: List[FieldDefinition]): FieldListRec = db.withTransaction {
    val fkRecs = fields.map(f => findOrCreateFieldKey(f.fieldKeyType, f.fieldKeyData))
    FieldListRec(fkRecs)
  }
}
