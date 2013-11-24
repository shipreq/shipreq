package com.beardedlogic.usecase
package app

import net.liftweb.common.Logger
import scalaz.{Name, Need}
import db.{DaoT, FieldListRec}
import feature.uc.field._

object Defaults extends Logger {

  val fieldListDefns: List[FieldDefinition] =
    TextFieldDefinition("Description") ::
      TextFieldDefinition("Actors") ::
      TextFieldDefinition("Pre-Conditions") ::
      TextFieldDefinition("Post-Conditions") ::
      NormalCourseFieldDefinition ::
      ExceptionCourseFieldDefinition ::
      FlowGraphFieldDefinition ::
      TextFieldDefinition("Use Case Relationships") ::
      TextFieldDefinition("Constraints and Business Rules") ::
      TextFieldDefinition("Frequency of Use") ::
      TextFieldDefinition("Special Requirements") ::
      TextFieldDefinition("Assumptions") ::
      TextFieldDefinition("Notes and Issues") ::
      Nil

  private var fieldList_ : Name[FieldListRec] = null
  def fieldList = fieldList_

  private def dbVal[V](fn: DaoT => V): Name[V] = Need(DI.DaoProvider.withTransaction(fn))

  def uninit(): Unit = {
    fieldList_ = dbVal(_.syncFieldList(fieldListDefns))
  }
  uninit()

  def init(): Unit = {
    fieldList.value
    debug("Defaults initialised successfully.")
  }
}