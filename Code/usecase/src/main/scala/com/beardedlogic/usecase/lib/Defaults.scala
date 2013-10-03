package com.beardedlogic.usecase
package lib

import net.liftweb.common.Logger
import scalaz.{Name, Need}
import field._
import db._

object Defaults extends Logger {

  /** Default title of new use cases. */
  val title = "Untitled"

  val useCaseHeader = UseCaseHeader(title)

  val fieldListDefns: List[FieldDefinition] =
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

  val fieldList: Name[FieldListRec] = dbVal(_.syncFieldList(fieldListDefns))

  private def dbVal[V](fn: DaoT => V): Name[V] = Need(DI.DaoProvider.withTransaction(fn))

  def init(): Unit = {
    fieldList.value
    debug("Defaults initialised successfully.")
  }
}