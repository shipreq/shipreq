package com.beardedlogic.usecase
package db

import scala.slick.jdbc.{GetResult, StaticQuery => Q}
import lib.Types._
import DBHelpers._

case class UcFieldTextWithFK(fkId: FieldKeyId, rel: UcFieldText) {
  @inline final def label = rel.label
  @inline final def parentId = rel.parentId
  @inline final def index = rel.index
  @inline final def textRev = rel.textRev
  @inline final def id = textRev.id
  @inline final def text = textRev.text
}

case class UcFieldText(label: Option[String], parentId: Option[TextRevId], index: Short, textRev: TextRev) {
  @inline final def id = textRev.id
  @inline final def text = textRev.text
}

object UcFieldAccessor {
  import TextAccessor.{tr_*, GRTextRev}

  implicit val GRUcFieldText= GetResult(r => UcFieldText(r.<<, r.<<, r.<<, r.<<))
  implicit val GRUcFieldTextWithFK = GetResult(r => UcFieldTextWithFK(r.<<, r.<<))

  val CopyUcFieldsBetweenRevs = Q.update[(UseCaseRevId, UseCaseRevId)]("""
    INSERT INTO uc_field
    SELECT ?, label, parent_rev_id, index, text_rev_id
      FROM uc_field where uc_rev_id = ?
  """.sql)

  // Step loading depends on ORDER BY index
  val SelectByUcRev = Q.query[UseCaseRevId, UcFieldTextWithFK](s"""
    SELECT fk_id, label, parent_rev_id, index, ${tr_*}
      FROM uc_field f, text_rev tr, text t
     WHERE text_rev_id = tr.id and tr.ident_id = t.id
       AND uc_rev_id = ?
       ORDER BY index
  """.sql)

  val InsertText = Q.update[(UseCaseRevId, TextRevId)]("INSERT INTO uc_field(uc_rev_id, text_rev_id) VALUES(?,?)")

  val InsertStep = Q.update[(UseCaseRevId, LabelStr, Option[TextRevId], Short, TextRevId)](
    "INSERT INTO uc_field(uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES(?,?,?,?,?)")
}

trait UcFieldAccessor extends DatabaseAccessor {
  import UcFieldAccessor._

  def linkUcToText(uc: UseCaseRevId, txt: TextRevId): Unit = InsertText.execute(uc, txt)

  def linkUcToStep(uc: UseCaseRevId, label: LabelStr, index: Short, parentId: Option[TextRevId], text: TextRev): UcFieldText = {
    InsertStep.execute(uc, label, parentId, index, text.id)
    UcFieldText(Some(label), parentId, index, text)
  }

  def copyUcFieldsBetweenRevs(from: UseCaseRevId, to: UseCaseRevId): Unit = CopyUcFieldsBetweenRevs.execute(to, from)

  def findAllUcFieldData(ucRevId: UseCaseRevId): List[UcFieldTextWithFK] = SelectByUcRev.list(ucRevId)
}