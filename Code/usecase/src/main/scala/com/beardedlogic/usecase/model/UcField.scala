package com.beardedlogic.usecase
package model

import scala.slick.jdbc.{GetResult, StaticQuery => Q}
import lib.db._
import lib.Types._
import DBHelpers._

case class UcFieldText(
  fkId: FieldKeyId,
  label: String,
  parentId: Option[TextRevId],
  index: Short,
  id: TextRevId,
  text: String)

object UcFieldAccessor {

  implicit val GRUcFieldText= GetResult(r => UcFieldText(
    r.nextId[FieldKeyId], r.<<, r.nextId_?[TextRevId], r.<<, r.nextId[TextRevId], r.<<))

  val CopyUcFieldsBetweenRevs = Q.update[(UseCaseRevId, UseCaseRevId)]("""
    INSERT INTO uc_field
    SELECT ?, label, parent_rev_id, index, text_rev_id
      FROM uc_field where uc_rev_id = ?
  """.sql)

  val SelectByUcRev = Q.query[UseCaseRevId, UcFieldText]("""
    SELECT fk_id, label, parent_rev_id, index, text_rev_id, text
      FROM uc_field f, text_rev r, text t
     WHERE text_rev_id = r.id and r.ident_id = t.id
       AND uc_rev_id = ?
       ORDER BY parent_rev_id DESC, index
  """.sql)
}

trait UcFieldAccessor extends DatabaseAccessor {
  import UcFieldAccessor._

  def copyUcFieldsBetweenRevs(from: UseCaseRevId, to: UseCaseRevId): Unit = CopyUcFieldsBetweenRevs.execute(to, from)

  def findAllUcFieldData(ucRevId: UseCaseRevId): List[UcFieldText] = SelectByUcRev.list(ucRevId)
}