package com.beardedlogic.usecase
package lib

import field._
import model._
import net.liftweb.common.Logger
import util.CachedFunction

/**
 * Data IDs below 100 are reserved and can be safely allocated here.
 */
object ReservedIds {

  val DefaultFieldList = 1
}

object Defaults extends Logger {

  /** Default title of new use cases. */
  val Title = "Untitled"

  /** This is only exposed for tests */
  val FieldListDefs: List[FieldDef[_]] =
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

  val FieldList = CachedFunction.eager1WithInitial[DAO, FieldList](dao => {
    val fl = dao.syncFieldList(ReservedIds.DefaultFieldList, FieldListDefs)
    debug(s"Default field list: ${fl.dataId}:${fl.valueId}")
    fl
  })(null)

  def init() {
    DI.DaoProvider.withTransaction { dao =>
      FieldList.refresh(dao)
    }
    debug("Defaults initialised successfully.")
  }
}