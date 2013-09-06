package com.beardedlogic.usecase
package db

import scala.slick.jdbc.{GetResult, StaticQuery => Q}
import lib.Types._
import DBHelpers._

case class FieldKeyRec(id: FieldKeyId, fkType: FieldKeyType, data: FieldKeyRecData) {
  def fieldDefn = fkType.fieldDefn(data)
  def field = fieldDefn.field(this)
}

// ---------------------------------------------------------------------------------------------------------------------

private[db] object FieldKeyAccessor {

  implicit val GetResultFieldKey = GetResult {r => FieldKeyRec(r.<<, r.<<, r.<<)}

  val SelectIdToReuse = Q.query[(FieldKeyType, FieldKeyRecData), FieldKeyId](
    "SELECT id FROM field_key WHERE type_id=? AND data IS NOT DISTINCT FROM ?")

  val Insert = Q.query[(FieldKeyType, FieldKeyRecData), FieldKeyId](
    "INSERT INTO field_key(type_id, data) VALUES(?,?) RETURNING id")
}

private[db] trait FieldKeyAccessor extends DatabaseAccessor {
  import FieldKeyAccessor._

  def findOrCreateFieldKey(fkType: FieldKeyType, data: FieldKeyRecData): FieldKeyRec = db.withTransaction {
    SelectIdToReuse.firstOption(fkType, data)
    .map(FieldKeyRec(_, fkType, data))
    .getOrElse(createFieldKey(fkType, data))
  }

  def createFieldKey(fkType: FieldKeyType, data: FieldKeyRecData): FieldKeyRec = {
    val id = Insert.first(fkType, data)
    FieldKeyRec(id, fkType, data)
  }
}