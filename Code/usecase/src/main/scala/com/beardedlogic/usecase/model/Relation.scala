package com.beardedlogic.usecase
package model

import scala.slick.driver.PostgresDriver.simple._
import scala.slick.jdbc.{StaticQuery => Q}
import lib.db._

trait RelationAccessor extends DatabaseAccessor {

  /**
   * This value is used in the `relation.index` field to indicate N/A, ie. the relationship doesn't require an index.
   */
  val INDEX_NA = -1.toShort

  def createRelationUnchecked(from: Value[_], relationType: RelationType, index: Short, to: Value[_]) {
    Q.update[(Long, Short, Short, Long)]("INSERT INTO relation VALUES(?,?,?,?)")
    .execute(from.valueId, relationType.ordinal, index, to.valueId)
  }

  @inline def fieldList_has_fieldKey(
    from: Value[DataType.FieldList],
    index: Short,
    to: Value[DataType.FieldKey]
    ) = createRelationUnchecked(from, RelationType.Has, index, to)

  @inline def stepParent_has_step(
    from: Value[_ <: StepParent],
    index: Short,
    to: Value[DataType.Step]
    ) = createRelationUnchecked(from, RelationType.Has, index, to)

  @inline def usecase_has_fieldValue(
    from: Value[DataType.UseCase],
    to: Value[DataType.FieldValue]
    ) = createRelationUnchecked(from, RelationType.Has, INDEX_NA, to)
}