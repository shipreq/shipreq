package com.beardedlogic.usecase
package model

import scala.slick.jdbc.{StaticQuery => Q}
import lib.db.DBHelpers._

trait RelationAccessor extends DatabaseAccessor {

  /**
   * This value is used in the `relation.index` field to indicate N/A, ie. the relationship doesn't require an index.
   */
  val INDEX_NA = -1.toShort

  val CreateTmpTableOfRecursiveHasRelations = Q.update[Long]( s"""
    CREATE TEMP TABLE tmp WITH (OIDS=FALSE) ON COMMIT DROP AS
    WITH RECURSIVE tmp2(from_id, type_id, index, to_id, path, cycle) AS (
      SELECT r.from_id, r.type_id, r.index, r.to_id, ARRAY[r.from_id], false
      FROM relation r
      WHERE r.from_id = ?
        and r.type_id = ${RelationType.Has.ordinal}
      UNION ALL
      SELECT r.from_id, r.type_id, r.index, r.to_id, path || r.from_id, r.from_id = ANY(path)
      FROM relation r, tmp2
      WHERE tmp2.to_id = r.from_id
        and r.type_id = ${RelationType.Has.ordinal}
        and not cycle
    )
    SELECT from_id, index, to_id FROM tmp2""".sql)

  val SelectFromTmpTableOfRecursiveHasRelations = Q.queryNA[(Long, Long)](
    "SELECT from_id, to_id FROM tmp ORDER BY index")

  val DropTmpTable = Q.updateNA("DROP TABLE tmp")

  def createRelationUnchecked(from: Value[_], relationType: RelationType, index: Short, to: Value[_]) {
    Q.update[(Long, Short, Short, Long)]("INSERT INTO relation VALUES(?,?,?,?)")
    .execute(from.valueId, relationType.ordinal, index, to.valueId)
  }

  @inline def relate_fieldList_has_fieldKey(
    from: Value[DataType.FieldList],
    index: Short,
    to: Value[DataType.FieldKey]
    ) = createRelationUnchecked(from, RelationType.Has, index, to)

  @inline def relate_stepParent_has_step(
    from: Value[_ <: StepParent],
    index: Short,
    to: Value[DataType.Step]
    ) = createRelationUnchecked(from, RelationType.Has, index, to)

  @inline def relate_usecase_has_fieldValue(
    from: Value[DataType.UseCase],
    to: Value[DataType.FieldValue]
    ) = createRelationUnchecked(from, RelationType.Has, INDEX_NA, to)

  /**
   * Filters a copy of the `relation` table into temp table, by children of a given value.
   *
   * Specifically, if a value `Has` 3 other values they will appear in the temp table, along with any values that
   * said 3 children have, along with their children, and so on.
   *
   * @param ownerId A value ID that will be used to filter the `from_id` column before recursing.
   * @param fn A function to invoke while the temp table is alive. The parameters will be
   *           1) the temp table name, and
   *           2) a map of from-IDs to to-IDs (in the order specified in the `index` column), taken from the temp table.
   * @return Whatever the given function returns.
   */
  def withTmpTableOfRecursiveHasRelations[T](ownerId: Long, fn: (String, Map[Long, List[Long]]) => T): T = db.withTransaction {

    CreateTmpTableOfRecursiveHasRelations.execute(ownerId)

    // Create map of from-IDs to to-IDs
    var relations = Map.empty[Long, List[Long]]
    SelectFromTmpTableOfRecursiveHasRelations.foreach { case (from, to) =>
      val newList = relations.getOrElse(from, List.empty[Long]) :+ to
      relations += (from -> newList)
    }
    relations

    val result = fn("tmp", relations)

    DropTmpTable.execute

    result
  }
}
