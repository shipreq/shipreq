package com.beardedlogic.shipreq
package app

import net.liftweb.common.Logger
import scalaz.{Name, Need}
import db.{DaoT, FieldListRec}
import feature.uc.field._

object Defaults extends Logger {

  val tfDescription     = TextFieldDefinition("Description")
  val tfActors          = TextFieldDefinition("Actors")
  val tfPreConditions   = TextFieldDefinition("Pre-Conditions")
  val tfPostConditions  = TextFieldDefinition("Post-Conditions")
  val tfUCRelationships = TextFieldDefinition("Use Case Relationships")
  val tfConstraints     = TextFieldDefinition("Constraints and Business Rules")
  val tfFreqOfUse       = TextFieldDefinition("Frequency of Use")
  val tfSpecialReqs     = TextFieldDefinition("Special Requirements")
  val tfAssumptions     = TextFieldDefinition("Assumptions")
  val tfNotesAndIssues  = TextFieldDefinition("Notes and Issues")

  val fieldListDefns: List[FieldDefinition] =
    tfDescription ::
    tfActors ::
    tfPreConditions ::
    tfPostConditions ::
    NormalCourseFieldDefinition ::
    ExceptionCourseFieldDefinition ::
    FlowGraphFieldDefinition ::
    tfUCRelationships ::
    tfConstraints ::
    tfFreqOfUse ::
    tfSpecialReqs ::
    tfAssumptions ::
    tfNotesAndIssues ::
    Nil

  private var fieldList_ : Name[FieldListRec] = null
  def fieldList = fieldList_

  private def dbVal[V](fn: DaoT => V): Name[V] = Need(DI.DaoProvider.vend.withTransaction(fn))

  def uninit(): Unit = {
    fieldList_ = dbVal(_.syncFieldList(fieldListDefns))
  }
  uninit()

  def init(): Unit = {
    fieldList.value
    debug("Defaults initialised successfully.")
  }
}