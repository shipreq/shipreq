package com.beardedlogic.usecase
package db

import org.joda.time.DateTime
import scala.slick.jdbc.{StaticQuery, SetParameter, GetResult}
import scala.slick.session.PositionedParameters
import lib.security.PasswordAndSalt
import lib.Types._

/**
 * SQL for all functions exposed in the DAO.
 */
private[db] final object Sql {
  import AutoExternaliseIds._
  import SqlHelpers._
  import StaticQuery.{query, queryNA, update, updateNA}

  implicit val GR_FieldKey = GetResult {r => FieldKeyRec(r.<<, r.<<, r.<<)}
  implicit val GR_PasswordAndSalt = GetResult(r => PasswordAndSalt(r.nextString, r.nextString))
  implicit val GR_ProjectSummary = GetResult(r => ProjectSummary(r.nextId[ProjectId], r.<<, r.<<, r.<<))
  implicit val GR_TextRev = GetResult(r => TextRev(r.<<, r.<<, r.<<, r.<<))
  implicit val GR_UcFieldText= GetResult(r => UcFieldText(r.<<, r.<<, r.<<, r.<<))
  implicit val GR_UcFieldTextWithFK = GetResult(r => UcFieldTextWithFK(r.<<, r.<<))
  implicit val GR_UseCaseIdent = GetResult {r => UseCaseIdent(r.<<, r.<<)}
  implicit val GR_UseCaseRev = GetResult(r => UseCaseRev(r.<<, r.<<, r.<<, UseCaseHeader(r.<<)))
  implicit val GR_UseCaseSummary = GetResult(r => UseCaseSummary(r.nextId[UseCaseIdentId], r.<<, r.<<, r.<<))
  implicit val GR_UserDescriptor = GetResult(r => UserDescriptor(r.<<, r.<<, r.<<))
  implicit val GR_UserRegistrationInfo = GetResult(r => UserRegistrationInfo(r.<<, r.<<, r.<<, r.<<))

  implicit object SP_PasswordAndSalt extends SetParameter[PasswordAndSalt] {
    def apply(v: PasswordAndSalt, pp: PositionedParameters) {
      pp.setString(v.hashedPassword)
      pp.setString(v.salt)
    }
  }

  private[this] case class Update() extends scala.annotation.StaticAnnotation
  private[this] case class Insert() extends scala.annotation.StaticAnnotation

  // ###################################################################################################################
  // User

  private val UserDescCols = "id,username,email"
  private val PwdAndSaltCols = "password, password_salt"

  val GetUserDescCredByUsername = query[String, (UserDescriptor, PasswordAndSalt)](
    s"SELECT $UserDescCols,$PwdAndSaltCols FROM usr WHERE username=?")

  val GetUserDescCredByEmail = query[String, (UserDescriptor, PasswordAndSalt)](
    s"SELECT $UserDescCols,$PwdAndSaltCols FROM usr WHERE email=? AND password IS NOT NULL")

  val GetUserRegInfo = query[String, UserRegistrationInfo](
    "SELECT id, confirmation_token, confirmation_sent_at, confirmed_at FROM usr WHERE email=?")

  val GetConfirmationTokenIssuedDate = query[String, DateTime](
    "SELECT confirmation_sent_at FROM usr WHERE confirmation_token=?")

  @Update val UpdateConfirmationToken = update[(String, UserId)](
    "UPDATE usr SET confirmation_token = ?, confirmation_sent_at = NOW() WHERE id=?")

  @Update val LogUserLogin = update[(String, UserId)](
    "UPDATE usr SET login_count = login_count + 1, last_login_at = NOW(), last_login_ip = ? WHERE id=?")

  @Insert val InsertUserPlaceholder = update[(String, String)](
    "INSERT INTO usr(email, confirmation_token, confirmation_sent_at) VALUES(?,?,NOW())")

  @Update val RegisterUser = query[(String, PasswordAndSalt, String, String), UserId]( """
    UPDATE usr SET username = ?
      ,password = ?, password_salt = ?, password_changed_at = NOW()
      ,confirmation_token = NULL, confirmed_at = NOW()
      ,login_count = 1, last_login_at = NOW(), last_login_ip = ?
    WHERE confirmation_token = ?
    RETURNING id""".sql)

  // ###################################################################################################################
  // Project

  @Insert val CreateProject = query[(UserId, String), ProjectId](
    "INSERT INTO project(usr_id, name) VALUES(?,?) RETURNING id")

  val SummariseProjects = query[UserId, ProjectSummary]( s"""
    SELECT p.id, p.name, count(r.created_at), to_iso8601_str(max(r.created_at))
    FROM project p
    LEFT JOIN usecase u on p.id = u.project_id
    LEFT JOIN usecase_rev r on r.id = u.latest_rev_id
    WHERE p.usr_id = ?
    GROUP BY p.id, p.name
    ORDER BY p.name """.sql)

  // ###################################################################################################################
  // Use Case

  private val ucrev_* = s"r.ident_id, u.number, r.rev, r.id, r.title"

  @Insert val InsertUseCaseIdent = {
    val NextUseCaseNumber = "(SELECT coalesce(max(number),0)+1 from usecase where project_id=?)"
    query[(ProjectId, ProjectId), UseCaseIdent](
      s"INSERT INTO usecase(project_id, number) VALUES(?, $NextUseCaseNumber) RETURNING id, number")
  }

  @Insert val InsertUseCaseIdentForceNum = query[(ProjectId, UseCaseNumber), UseCaseIdentId](
    "INSERT INTO usecase(project_id,number) VALUES(?,?) RETURNING id")

  @Insert val InsertUseCaseRev = query[(UseCaseIdentId, Short, String), UseCaseRevId](
    "INSERT INTO usecase_rev(ident_id, rev, title) VALUES(?,?,?) RETURNING id")

  val SelectUseCaseRev = query[UseCaseRevId, UseCaseRev](
    s"SELECT ${ucrev_*} FROM usecase u, usecase_rev r WHERE u.id=r.ident_id AND r.id=?")

  val SelectLatestUseCaseRevId = query[UseCaseIdentId, UseCaseRevId](
    s"SELECT latest_rev_id FROM usecase WHERE id=?")

  val SelectLatestUseCaseRev = query[UseCaseIdentId, UseCaseRev](
    s"SELECT ${ucrev_*} FROM usecase u, usecase_rev r WHERE r.id=latest_rev_id AND u.id=?")

  val SummariseUseCases = query[ProjectId, UseCaseSummary]( s"""
    SELECT ident_id, number, title, to_iso8601_str(created_at)
    FROM usecase u, usecase_rev r
    WHERE r.id = latest_rev_id and project_id = ?
    ORDER BY number """.sql)

  @Update val UpdateUseCaseTitleDirect = update[(String, UseCaseRevId)]("UPDATE usecase_rev SET title=? WHERE id=?")

  // ###################################################################################################################
  // Text

  private val textrev_* = "tr.ident_id, tr.rev, tr.id, tr.text"

  @Insert val InsertTextIdent = query[(UseCaseIdentId, FieldKeyId), TextIdentId](
    "INSERT INTO text(uc_id, fk_id) VALUES(?,?) RETURNING id")

  @Insert val InsertTextRev = query[(TextIdentId, Short, TextWithNormalisedRefs), TextRevId](
    "INSERT INTO text_rev(ident_id, rev, text) VALUES(?,?,?) RETURNING id")

  // ###################################################################################################################
  // uc_field

  @Insert val LinkUcToText = update[(UseCaseRevId, TextRevId)](
    "INSERT INTO uc_field(uc_rev_id, text_rev_id) VALUES(?,?)")

  @Insert val LinkUcToStep = update[(UseCaseRevId, LabelStr, Option[TextRevId], Short, TextRevId)](
    "INSERT INTO uc_field(uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES(?,?,?,?,?)")

  @Insert val CopyUcFieldsBetweenRevs = update[(UseCaseRevId, UseCaseRevId)]("""
    INSERT INTO uc_field
    SELECT ?, label, parent_rev_id, index, text_rev_id
      FROM uc_field where uc_rev_id = ? """.sql)

  // Step loading depends on ORDER BY index
  val SelectUcFields = query[UseCaseRevId, UcFieldTextWithFK](s"""
    SELECT fk_id, label, parent_rev_id, index, ${textrev_*}
      FROM uc_field f, text_rev tr, text t
     WHERE text_rev_id = tr.id and tr.ident_id = t.id
       AND uc_rev_id = ?
     ORDER BY index """.sql)

  // ###################################################################################################################
  // Fields

  val SelectReusableFieldKeyId = query[(FieldKeyType, FieldKeyRecData), FieldKeyId](
    "SELECT id FROM field_key WHERE type_id=? AND data IS NOT DISTINCT FROM ?")

  @Insert val InsertFieldKey = query[(FieldKeyType, FieldKeyRecData), FieldKeyId](
    "INSERT INTO field_key(type_id, data) VALUES(?,?) RETURNING id")
}
