package com.beardedlogic.usecase
package model

import scala.slick.driver.PostgresDriver.simple._
import scala.slick.jdbc.{StaticQuery => Q}
import lib.db._
import DBHelpers._

object Relation extends DBTable {
  override val TableName = "relation"

  def createUnchecked(from: Value[_], relationType: RelationType, index: Short, to: Value[_])(implicit s: Session) {
    Q.update[(Long, Short, Short, Long)]("INSERT INTO relation VALUES(?,?,?,?)")
    .execute(from.valueId, relationType.ordinal, index, to.valueId)
  }

  // FieldList --[Has]--> FieldKey
  @inline def create(
    from: Value[DataType.FieldList],
    relationType: RelationType.Has, index: Short,
    to: Value[DataType.FieldKey]
    )(implicit s: Session) = createUnchecked(from, relationType, index, to)
}