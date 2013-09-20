package com.beardedlogic.usecase
package lib

import net.liftweb.common.Logger
import field._
import db._
import util.LazyVal

object Defaults extends Logger {

  /** Default title of new use cases. */
  val Title = "Untitled"

  val FieldListDefns: List[FieldDefinition] =
    TextFieldDefinition("Description") ::
      TextFieldDefinition("Actors") ::
      TextFieldDefinition("Pre-Conditions") ::
      TextFieldDefinition("Post-Conditions") ::
      NormalCourseFieldDefinition ::
      ExceptionCourseFieldDefinition ::
      TextFieldDefinition("Use Case Relationships") ::
      TextFieldDefinition("Constraints and Business Rules") ::
      TextFieldDefinition("Frequency of Use") ::
      TextFieldDefinition("Special Requirements") ::
      TextFieldDefinition("Assumptions") ::
      TextFieldDefinition("Notes and Issues") ::
      Nil

  val FieldList: LazyVal[FieldListRec] = LazyDbVal(_.syncFieldList(FieldListDefns))

  private def LazyDbVal[V](fn: Dao => V) = LazyVal <~ DI.DaoProvider.withTransaction(fn)

  def init(): Unit = {
    FieldList.get
    debug("Defaults initialised successfully.")
  }
}