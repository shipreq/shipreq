package com.beardedlogic.usecase.model

import scala.slick.jdbc.{StaticQuery => Q}

object StepAccessor {
  val Insert = Q.update[(Long, String)]("INSERT INTO step VALUES(?,?)")
}

trait StepAccessor extends DatabaseAccessor {

  import StepAccessor._

  def createStep(value: Value[DataType.Step], text: String): Unit = {
    Insert.execute(value.valueId, text)
  }

  def mapStepTextById(sqlCond: String): Map[Long, String] = {
    val map = Map.newBuilder[Long, String]
    Q.queryNA[(Long, String)]("SELECT id,text FROM step " + sqlCond).foreach(map += _)
    map.result
  }
}

/**
 * Marks a `DataType` as being allow to link to a `Step` with a `Has` relationship.
 */
trait StepParent {
  self: DataType =>
}