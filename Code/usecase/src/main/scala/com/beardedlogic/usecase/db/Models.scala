package com.beardedlogic.usecase
package db

import org.joda.time.DateTime
import scala.reflect.ClassTag
import lib.Types._
import lib.UcChangeDomain
import lib.field._

// ===================================================================================================================
// User

case class UserDescriptor(id: UserId, username: String, email: String)

case class UserRegistrationInfo(
  id: UserId,
  confirmationToken: Option[String],
  confirmationSentAt: Option[DateTime],
  confirmedAt: Option[DateTime])

// ===================================================================================================================
// Fields

case class FieldKeyRec(id: FieldKeyId, fkType: FieldKeyType, data: FieldKeyRecData) {
  def fieldDefn = fkType.fieldDefn(data)
  def field = fieldDefn.field(this)
}

case class FieldListRec(fieldKeys: List[FieldKeyRec]) {
  val fields = fieldKeys.map(_.field)
  def fieldDefns = fieldKeys.map(_.fieldDefn)

  def filterFields[T <: Field](implicit m: ClassTag[T]): List[T] = fields.filter {case f: T => true; case _ => false}.asInstanceOf[List[T]]
  lazy val NCF: NormalCourseField = filterFields[NormalCourseField].head
  lazy val ECF: ExceptionCourseField = filterFields[ExceptionCourseField].head
  lazy val textFields: List[TextField] = filterFields[TextField]
}

object FieldListRec {
  def fromFields(fields: List[Field]) = apply(fields map (_.rec))
}

// ===================================================================================================================
// UC & Text

case class UseCaseIdent(identId: UseCaseIdentId, number: UseCaseNumber)

case class UseCaseRev(ident: UseCaseIdent, rev: Short, id: UseCaseRevId, header: UseCaseHeader) {
  @inline final def identId = ident.identId
}

case class UseCaseHeader(title: String @@ Validated)
object UseCaseHeader extends UcChangeDomain

case class TextRev(identId: TextIdentId, rev: Short, id: TextRevId, text: TextWithNormalisedRefs)

case class UcFieldTextWithFK(fkId: FieldKeyId, rel: UcFieldText) {
  @inline final def label = rel.label
  @inline final def parentId = rel.parentId
  @inline final def index = rel.index
  @inline final def textRev = rel.textRev
  @inline final def id = textRev.id
  @inline final def text = textRev.text
}

case class UcFieldText(label: Option[StepLabel], parentId: Option[TextRevId], index: Short, textRev: TextRev) {
  @inline final def id = textRev.id
  @inline final def text = textRev.text
}

// ===================================================================================================================
// Project

case class Project(id: ProjectId, name: String, owner: UserId)