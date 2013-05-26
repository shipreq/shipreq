package com.beardedlogic.usecase
package lib

import field._
import model._
import db.DB
import net.liftweb.common.Logger

/**
 * Data IDs below 100 are reserved and can be safely allocated here.
 */
object ReservedIds {

  val DefaultFieldList = 1
}

object Defaults extends Logger {
  private[this] var dao = DAO.get

  val FieldList = dao.withTransaction {
    //  val DateCreated = TextFieldDef("Date Created")
    //  val DateLastUpdated = TextFieldDef("Date Last Updated")
    val fields: List[FieldDef] =
      TextFieldDef("Actors") ::
        TextFieldDef("Pre-Conditions") ::
        TextFieldDef("Post-Conditions") ::
        NormalAndAlternateCourseFields ::
        ExceptionCourseFields ::
        TextFieldDef("Use Case Relationships") ::
        TextFieldDef("Constraints and Business Rules") ::
        TextFieldDef("Frequency of Use") ::
        TextFieldDef("Special Requirements") ::
        TextFieldDef("Assumptions") ::
        TextFieldDef("Notes and Issues") ::
        Nil

    dao.syncFieldList(ReservedIds.DefaultFieldList, fields)
  }
  debug(s"Default field list: ${FieldList.dataId}/${FieldList.valueId}")

  dao.close()
  dao= null

  def init() {
    debug("Defaults initialised successfully.")
  }
}