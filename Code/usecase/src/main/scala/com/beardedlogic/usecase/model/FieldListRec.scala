package com.beardedlogic.usecase
package model

import scala.reflect.ClassTag
import com.beardedlogic.usecase.lib.field._

case class FieldListRec(fieldKeys: List[FieldKeyRec]) {
  val fields = fieldKeys.map(_.field)
  def fieldDefns = fieldKeys.map(_.fieldDefn)

  def filterFields[T <: Field](implicit m: ClassTag[T]): List[T] = fields.filter {case f: T => true; case _ => false}.asInstanceOf[List[T]]
  lazy val NCF: NormalCourseField = filterFields[NormalCourseField].head
  lazy val ECF: ExceptionCourseField = filterFields[ExceptionCourseField].head
  lazy val textFields: List[TextField] = filterFields[TextField]
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
