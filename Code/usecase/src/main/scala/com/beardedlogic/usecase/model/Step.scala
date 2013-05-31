package com.beardedlogic.usecase
package model

import scala.slick.jdbc.{StaticQuery => Q, GetResult}
import lib.TypeTags._
import StepAccessor._

object StepAccessor {
  val Insert = Q.update[(Long, String)]("INSERT INTO step VALUES(?,?)")
  implicit val GetResultPlainValue = ValueAccessor.GetValueResult[DataType.Step]
}

trait StepAccessor extends DatabaseAccessor {

  def createStep(value: Value[DataType.Step], text: String): Unit = {
    Insert.execute(value.valueId, text)
  }

  def findAllStepValuesAndText(sqlCond: String): Map[Long_StepValueId, (PlainValue[DataType.Step], String)] = {
    val map = Map.newBuilder[Long_StepValueId, (PlainValue[DataType.Step], String)]
    Q.queryNA[(PlainValue[DataType.Step], String)](
      s"SELECT v.${ValueAccessor.*}, text FROM step s INNER JOIN value v on v.id=s.id ${sqlCond}"
    ).foreach(r => map += (tag[StepValueId](r._1.valueId) -> r))
    map.result
  }
}

/**
 * Marks a `DataType` as being allow to link to a `Step` with a `Has` relationship.
 */
trait StepParent {
  self: DataType =>
}