package com.beardedlogic.usecase
package db

import scala.slick.jdbc.{GetResult, StaticQuery => Q}
import lib.Types._
import DBHelpers._

case class TextRev(identId: TextIdentId, rev: Short, id: TextRevId, text: TextWithNormalisedRefs)

private[db] object TextAccessor {

  val tr_* = "tr.ident_id, tr.rev, tr.id, tr.text"

  implicit val GRTextRev = GetResult(r => TextRev(r.<<, r.<<, r.<<, r.<<))

  val InsertIdent = Q.query[(UseCaseIdentId, FieldKeyId), TextIdentId](
    "INSERT INTO text(uc_id, fk_id) VALUES(?,?) RETURNING id")

  val InsertRev = Q.query[(TextIdentId, Short, TextWithNormalisedRefs), TextRevId](
    "INSERT INTO text_rev(ident_id, rev, text) VALUES(?,?,?) RETURNING id")
}

private[db] trait TextAccessor extends DatabaseAccessor {
  import TextAccessor._

  def createInitialText(ucId: UseCaseIdentId, fkId: FieldKeyId): TextIdentId = InsertIdent.first(ucId, fkId)

  def createTextRev(identId: TextIdentId, rev: Short, text: TextWithNormalisedRefs): TextRev = {
    val id = InsertRev.first(identId, rev, text)
    TextRev(identId, rev, id, text)
  }

}