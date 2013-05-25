package com.beardedlogic.usecase.model

import com.beardedlogic.usecase.lib.db.DBTable
import scala.slick.driver.PostgresDriver.simple._
import scala.slick.jdbc.{StaticQuery => Q}

object Step extends DBTable {

  override val TableName = "step"

  private val Insert = Q.update[(Long, String)](
    s"INSERT INTO $TableName VALUES(?,?)")

  def create(value: Value[DataType.Step], text: String)(implicit s: Session): Unit = {
    Insert.execute(value.valueId, text)
  }
}

/**
 * Marks a `DataType` as being allow to link to a `Step` with a `Has` relationship.
 */
trait StepParent {
  self: DataType =>
}