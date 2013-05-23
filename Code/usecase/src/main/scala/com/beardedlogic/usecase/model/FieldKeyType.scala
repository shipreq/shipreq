package com.beardedlogic.usecase
package model

import lib.db.DatabaseEnum
import lib.field._
import FieldKey.FieldKeyData

/**
 * Represents types of fields that a use case (or something else) can have.
 *
 * @since 22/05/2013
 */
sealed abstract class FieldKeyType(val ordinal: Short) extends FieldKeyType.Value {
  def fieldDef(data: FieldKeyData): FieldDef
}

object FieldKeyType extends DatabaseEnum[FieldKeyType] {
  override val TableName = "field_key_type"

  /**
   * A field with a name and a single text value.
   */
  case object Text extends FieldKeyType(300) {
    override def fieldDef(data: FieldKeyData) = TextFieldDef(data.get)
  }

  /**
   * A composite field of Normal Course, and Alternate Course use case step trees.
   */
  case object NormalAndAlternateCourses extends FieldKeyType(301) {
    override def fieldDef(data: FieldKeyData) = NormalAndAlternateCourseFields
  }

  /**
   * A field of Exception Course use case step trees.
   */
  case object ExceptionCourses extends FieldKeyType(302) {
    override def fieldDef(data: FieldKeyData) = ExceptionCourseFields
  }

  val Values = List(Text, NormalAndAlternateCourses, ExceptionCourses)
}
