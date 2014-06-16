package shipreq.webapp
package db

import org.joda.time.DateTime
import shipreq.base.util.TaggedTypes.JsonStr
import scala.reflect.ClassTag
import shipreq.taskman.api.{EmailAddr, UserId}
import lib.Types._
import feature.{UcFilter, ExternalId}
import feature.uc.field._
import feature.uc.UseCaseFns
import security.PasswordAndSalt

// ===================================================================================================================
// User

case class UserDescriptor(id: UserId, username: Username, email: EmailAddr, roles: Set[String]) {
  final def hasRole(role: String): Boolean = roles.contains(role)
}

object UserDescriptor {
  def roleStr(roles: Set[String]): Option[String] =
    if (roles.isEmpty)
      None
    else
      Some(roles.mkString(","))
}

case class UserDetail(name: String, newsletter: Boolean)

case class UserSupplementalInfo(ps: PasswordAndSalt, registeredAt: ISO8601)

case class UserRegistrationInfo(
  id: UserId,
  confirmationToken: Option[String],
  confirmationSentAt: Option[DateTime],
  confirmedAt: Option[DateTime])

case class ResetPasswordInfo(token: Option[String], sentAt: Option[DateTime])

case class UsrCount(registered: Long, total: Long) {
  def pending = total - registered
}

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

sealed trait BasicUseCaseInfo {
  def identId: UseCaseIdentId
  def number: UseCaseNumber
  def title: String
  final def eid = ExternalId.UseCase.toExternal(identId)
  final def fullName = UseCaseFns.fullName(number, title)
}

case class UseCaseSummary(
  id: UseCaseIdentId,
  number: UseCaseNumber,
  title: String,
  updatedAt: ISO8601) extends BasicUseCaseInfo {
  @inline final def identId = id
  def this(ucr: UseCaseRev, updatedAt: ISO8601) = this(ucr.identId, ucr.ident.number, ucr.title, updatedAt)
}

case class UseCaseIdent(identId: UseCaseIdentId, number: UseCaseNumber, projectId: ProjectId)

case class UseCaseRev(ident: UseCaseIdent, rev: Short, id: UseCaseRevId, header: UseCaseHeader, createdAt: DateTime)
  extends BasicUseCaseInfo {
  @inline final def projectId = ident.projectId
  @inline final def identId = ident.identId
  @inline final def number = ident.number
  @inline final def title = header.title
}

case class UseCaseHeader(title: String)

case class TextRev(identId: TextIdentId, rev: Short, id: TextRevId, text: NormalisedText)

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

case class ProjectSummary(
  id: ProjectId,
  name: String,
  ucCount: Long,
  ucUpdatedAt: Option[ISO8601],
  shareCount: Long,
  shareViews: Long,
  shareLastViewedAt: Option[ISO8601])

// ===================================================================================================================
// Shares

case class Share(
  id: ShareId,
  projectId: ProjectId,
  urlToken: ShareUrlToken,
  name: String,
  preface: Option[String],
  ucFilterJson: JsonStr[UcFilter]) {
  def ucFilter = UcFilter.fromJson(ucFilterJson)
}

case class ShareSummary(
  id: ShareId,
  urlToken: ShareUrlToken,
  name: String,
  ucFilterJson: JsonStr[UcFilter],
  viewCount: Long,
  lastViewedAt: Option[ISO8601]) {
  def ucFilter = UcFilter.fromJson(ucFilterJson)
}
