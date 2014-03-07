package shipreq.webapp
package db

import org.joda.time.DateTime
import scala.slick.jdbc.{StaticQuery, SetParameter, GetResult}
import slick.session.{PositionedResult, PositionedParameters}
import scalaz.NonEmptyList
import lib.ScalazSubset._
import lib.Types._
import feature.UcFilter
import security.PasswordAndSalt
import SqlHelpers._
import StaticQuery.{query, queryNA, update, updateNA}

/**
 * SQL for all functions exposed in the DAO.
 */
private[db] final object Sql {
  implicit def autotag[T <: AnyRef](t: T): T @@ Validated = t.tag[Validated]

  def userRoles(r: PositionedResult): Set[String] =
    r.nextStringOption match {
      case None        => Set.empty
      case Some(roles) => roles.split(',').toSet
    }

  implicit val GR_FieldKey = GetResult {r => FieldKeyRec(r.<<, r.<<, r.<<)}
  implicit val GR_PasswordAndSalt = GetResult(r => PasswordAndSalt.restore(r.nextString.tag, r.<<))
  implicit val GR_Project = GetResult(r => Project(r.<<, r.<<, r.<<))
  implicit val GR_ProjectSummary = GetResult(r => ProjectSummary(r.nextId[ProjectId], r.<<, r.<<, r.<<, r.<<, r.<<, r.<<))
  implicit val GR_TextRev = GetResult(r => TextRev(r.<<, r.<<, r.<<, r.<<))
  implicit val GR_UcFieldText= GetResult(r => UcFieldText(r.nextStringOption.asLabelC, r.<<, r.<<, r.<<))
  implicit val GR_UcFieldTextWithFK = GetResult(r => UcFieldTextWithFK(r.<<, r.<<))
  implicit val GR_UseCaseIdent = GetResult {r => UseCaseIdent(r.<<, r.<<, r.<<)}
  implicit val GR_UseCaseRev = GetResult(r => UseCaseRev(r.<<, r.<<, r.<<, UseCaseHeader(r.nextString), r.<<))
  implicit val GR_UseCaseSummary = GetResult(r => UseCaseSummary(r.nextId[UseCaseIdentId], r.<<, r.<<, r.<<))
  implicit val GR_UserDescriptor = GetResult(r => UserDescriptor(r.<<, r.<<, r.<<, userRoles(r)))
  implicit val GR_UserRegistrationInfo = GetResult(r => UserRegistrationInfo(r.<<, r.<<, r.<<, r.<<))
  implicit val GR_ResetPasswordInfo = GetResult(r => ResetPasswordInfo(r.<<, r.<<))
  implicit val GR_UserSupplementalInfo = GetResult(r => UserSupplementalInfo(r.<<, r.<<))

  implicit val GR_Share = GetResult(r => Share(r.<<, r.<<, r.<<, r.<<, r.<<, r.<<))
  implicit val GR_ShareSummary = GetResult(r => ShareSummary(r.<<, r.<<, r.<<, r.<<, r.<<, r.<<))

  implicit object SP_PasswordAndSalt extends SetParameter[PasswordAndSalt] {
    def apply(v: PasswordAndSalt, pp: PositionedParameters) {
      pp.setString(v.hashedPassword)
      pp.setString(v.salt)
    }
  }

  private[this] case class Insert() extends scala.annotation.StaticAnnotation
  private[this] case class Update() extends scala.annotation.StaticAnnotation
  private[this] case class Delete() extends scala.annotation.StaticAnnotation

  private def idsToSql(ids: NonEmptyList[JLong]): String = ids.map(_.toString).intercalate(",")

  // ###################################################################################################################
  // User

  private val UserDescCols = "id,username,email,roles"
  private val PwdAndSaltCols = "password,password_salt"
  private val UserRegistrationInfoCols = "id,confirmation_token,confirmation_sent_at,confirmed_at"

  val GetUserDescCredByUsername = query[String, (UserDescriptor, PasswordAndSalt)](
    s"SELECT $UserDescCols,$PwdAndSaltCols FROM usr WHERE username=?")

  val GetUserDescCredByEmail = query[String, (UserDescriptor, PasswordAndSalt)](
    s"SELECT $UserDescCols,$PwdAndSaltCols FROM usr WHERE email=? AND password IS NOT NULL")

  val GetUserRegInfo = query[String, UserRegistrationInfo](
    s"SELECT $UserRegistrationInfoCols FROM usr WHERE email=?")

  val GetUserRegAndResetPwInfo = query[String, (UserRegistrationInfo, ResetPasswordInfo)](
    s"SELECT $UserRegistrationInfoCols, reset_password_token, reset_password_sent_at FROM usr WHERE email=?")

  val GetConfirmationTokenIssuedDate = query[String, DateTime](
    "SELECT confirmation_sent_at FROM usr WHERE confirmation_token=?")

  @Update val UpdateConfirmationToken = update[(String, UserId)](
    "UPDATE usr SET confirmation_token = ?, confirmation_sent_at = NOW() WHERE id=?")

  @Insert val LogUserLogin = update[(UserId, Option[String])](
    "INSERT INTO usr_login_log(usr_id,ip) VALUES(?,?)")

  @Insert val InsertUserPlaceholder = update[(String, String)](
    "INSERT INTO usr(email, confirmation_token, confirmation_sent_at) VALUES(?,?,NOW())")

  @Update val RegisterUser = query[(String, PasswordAndSalt, String, String), UserId]( """
    UPDATE usr SET username = ?
      ,password = ?, password_salt = ?, password_changed_at = NOW()
      ,confirmation_token = NULL, confirmed_at = NOW()
      ,login_count = 1, last_login_at = NOW(), last_login_ip = ?
    WHERE confirmation_token = ?
    RETURNING id""".sql)

  val GetUserSupplementalInfo = query[UserId, UserSupplementalInfo](
    "SELECT password, password_salt, confirmed_at FROM usr WHERE id=?")

  @Update val UpdateUserPassword = update[(PasswordAndSalt, UserId)](
    "UPDATE usr SET password = ?, password_salt = ?, password_changed_at = NOW() WHERE id=?")

  @Update val InstallNewResetPasswordToken = update[(String, UserId)](
    "UPDATE usr SET reset_password_token = ?, reset_password_sent_at = NOW(), reset_password_req_count = reset_password_req_count + 1 WHERE id=?")

  @Update val ReuseResetPasswordToken = update[UserId](
    "UPDATE usr SET reset_password_sent_at = NOW(), reset_password_req_count = reset_password_req_count + 1 WHERE id=?")

  val GetResetPasswordTokenIssuedDate = query[String, DateTime](
    "SELECT reset_password_sent_at FROM usr WHERE reset_password_token=?")

  @Update val ResetPassword = update[(PasswordAndSalt, String)]("""
    UPDATE usr SET
      password = ?, password_salt = ?, password_changed_at = NOW(),
      reset_password_token = NULL
    WHERE reset_password_token = ? """.sql)

  // ###################################################################################################################
  // Project

  @Insert val CreateProject = query[(UserId, String), ProjectId](
    "INSERT INTO project(usr_id, name) VALUES(?,?) RETURNING id")

  private val project_* = s"id,name,usr_id"
  private val projectIsDead = "deleted_at IS NOT NULL"
  private val projectIsLive = "deleted_at IS NULL"

  val FindProject = query[ProjectId, Project](s"SELECT ${project_*} FROM project WHERE id=? AND $projectIsLive")

  val FindProjectByUc = query[UseCaseIdentId, Project](
    s"SELECT ${project_*} FROM project WHERE id = (SELECT project_id FROM usecase u WHERE u.id=?) AND $projectIsLive")

  @Update val RenameProject = update[(String, ProjectId, UserId)](
    "UPDATE project SET name=? WHERE id=? AND usr_id=?")

  val SummariseProjects = query[UserId, ProjectSummary]( s"""
    with projects as (
      select id, name from project where usr_id = ? and $projectIsLive
    ), ucs as (
      select p.id
        ,count(1) uc_count
        ,to_iso8601_str(max(r.created_at)) uc_last_updated_at
      from projects p
      inner join usecase     u on u.project_id = p.id
      inner join usecase_rev r on r.id = u.latest_rev_id
      group by p.id
    ), shares as (
      select p.id
        ,count(1) sh_count
        ,sum(s.view_count) sh_views
        ,to_iso8601_str(max(s.last_viewed_at)) sh_last_viewed_at
      from projects p, share s
      where p.id = s.project_id
      group by p.id
    )
    select
      p.id
      ,p.name
      ,uc_count
      ,uc_last_updated_at
      ,sh_count
      ,sh_views
      ,sh_last_viewed_at
    from projects p
    left join ucs    u on u.id = p.id
    left join shares s on s.id = p.id
    order by p.name
    """.sql)

  @Update val DeleteProjectSoft = update[(String, ProjectId)]("UPDATE project SET name=?, deleted_at=NOW() where id=?")
  @Delete val DeleteProjectHard = update[ProjectId](s"DELETE FROM project where id=? and $projectIsDead")

  // ###################################################################################################################
  // Use Case

  private val ucrev_* = s"r.ident_id, u.number, u.project_id, r.rev, r.id, r.title, r.created_at"
  //private val ucrevS_* = ucrev_*.replaceAll("((?:[a-z]+\\.)created_at)", "to_iso8601_str($1)")

  @Insert val InsertUseCaseIdent = {
    val NextUseCaseNumber = "(SELECT coalesce(max(number),0)+1 from usecase where project_id=?)"
    query[(ProjectId, ProjectId), UseCaseIdent](
      s"INSERT INTO usecase(project_id, number) VALUES(?, $NextUseCaseNumber) RETURNING id, number, project_id")
  }

  @Insert val InsertUseCaseIdentForceNum = query[(ProjectId, UseCaseNumber), UseCaseIdentId](
    "INSERT INTO usecase(project_id,number) VALUES(?,?) RETURNING id")

  @Insert val InsertUseCaseRev = query[(UseCaseIdentId, Short, String), (UseCaseRevId, DateTime)](
    "INSERT INTO usecase_rev(ident_id, rev, title) VALUES(?,?,?) RETURNING id, created_at")

  val SelectUseCaseRev = query[UseCaseRevId, UseCaseRev](
    s"SELECT ${ucrev_*} FROM usecase u, usecase_rev r WHERE u.id=r.ident_id AND r.id=?")

  val SelectLatestUseCaseRevId = query[UseCaseIdentId, UseCaseRevId](
    s"SELECT latest_rev_id FROM usecase WHERE id=?")

  val SelectLatestUseCaseRev = query[UseCaseIdentId, UseCaseRev](
    s"SELECT ${ucrev_*} FROM usecase u, usecase_rev r WHERE r.id=latest_rev_id AND u.id=?")

  val SelectLatestUseCaseRevsByProject =
    query[ProjectId, UseCaseRev](summariseUseCaseSql(ucrev_*))

  val SelectLatestUseCaseRevsByIds =
    query[(ProjectId, List[UseCaseIdentId]), UseCaseRev](summariseUseCaseSql(ucrev_*, Some("ident_id = ANY(?)")))

  private def summariseUseCaseSql(select: String, where: Option[String] = None) = {
    val w = where match {
      case None       => ""
      case Some(cond) => s" and ($cond)"
    }
    s"SELECT $select FROM usecase u, usecase_rev r WHERE r.id = latest_rev_id and project_id = ?$w ORDER BY number"
  }

  val SummariseUseCases = query[ProjectId, UseCaseSummary](
    summariseUseCaseSql("ident_id, number, title, to_iso8601_str(created_at)"))

  // ###################################################################################################################
  // Text

  private val textrev_* = "tr.ident_id, tr.rev, tr.id, tr.text"

  @Insert val InsertTextIdent = query[(UseCaseIdentId, FieldKeyId), TextIdentId](
    "INSERT INTO text(uc_id, fk_id) VALUES(?,?) RETURNING id")

  @Insert val InsertTextRev = query[(TextIdentId, Short, NormalisedText), TextRevId](
    "INSERT INTO text_rev(ident_id, rev, text) VALUES(?,?,?) RETURNING id")

  // ###################################################################################################################
  // uc_field

  @Insert val LinkUcToText = update[(UseCaseRevId, TextRevId)](
    "INSERT INTO uc_field(uc_rev_id, text_rev_id) VALUES(?,?)")

  @Insert val LinkUcToStep = update[(UseCaseRevId, StepLabel, Option[TextRevId], Short, TextRevId)](
    "INSERT INTO uc_field(uc_rev_id, label, parent_rev_id, index, text_rev_id) VALUES(?,?,?,?,?)")

  @Insert val CopyUcFieldsBetweenRevs = update[(UseCaseRevId, UseCaseRevId)]("""
    INSERT INTO uc_field
    SELECT ?, label, parent_rev_id, index, text_rev_id
      FROM uc_field where uc_rev_id = ? """.sql)

  private def selectUcFieldSql(selectPrefix: String, cond: String) = s"""
    SELECT ${selectPrefix}fk_id, label, parent_rev_id, index, ${textrev_*}
      FROM uc_field f, text_rev tr, text t
     WHERE text_rev_id = tr.id and tr.ident_id = t.id
       AND $cond
     ORDER BY index """.sql
  // ORDER BY: Step loading RELIES on ORDER BY index

  val SelectUcFields = query[UseCaseRevId, UcFieldTextWithFK](
    selectUcFieldSql("","uc_rev_id = ?"))

  val SelectUcFieldsInBulk = query[List[UseCaseRevId], (UseCaseRevId, UcFieldTextWithFK)](
    selectUcFieldSql("uc_rev_id,", "uc_rev_id = ANY(?)"))

  // ###################################################################################################################
  // Fields

  val SelectReusableFieldKeyId = query[(FieldKeyType, FieldKeyRecData), FieldKeyId](
    "SELECT id FROM field_key WHERE type_id=? AND data IS NOT DISTINCT FROM ?")

  @Insert val InsertFieldKey = query[(FieldKeyType, FieldKeyRecData), FieldKeyId](
    "INSERT INTO field_key(type_id, data) VALUES(?,?) RETURNING id")

  // ###################################################################################################################
  // Shares

  @Insert val InsertShare = query[(ProjectId, ShareUrlToken, PasswordAndSalt, String, Option[String], Json[UcFilter]), ShareId](
    "INSERT INTO share(project_id, url_token, password, password_salt, name, preface, uc_filter)"
      + " VALUES(?,?,?,?,?,?,?) RETURNING id")

  @Update val UpdateShare = update[(String, Option[String], Json[UcFilter], ShareId)](
    "UPDATE share SET name=?, preface=?, uc_filter=? WHERE id=?")

  @Update val UpdateSharePassword = update[(PasswordAndSalt, ShareId)](
    "UPDATE share SET password=?, password_salt=?, password_changed_at=NOW() WHERE id=?")

  @Delete val DeleteShare = update[ShareId]("DELETE FROM share WHERE id=?")

  @Insert val LogShareView = update[(ShareId, Option[String])]("INSERT INTO share_view_log(share_id,ip) VALUES(?,?)")

  val share_* = "id, project_id, url_token, name, preface, uc_filter"

  val SelectShare = query[ShareId, Share](
    s"SELECT ${share_*} FROM share WHERE id=?")

  val SelectShareAndPasswordByUrl = query[ShareUrlToken, (Share, PasswordAndSalt)](
    s"SELECT ${share_*}, password, password_salt FROM share WHERE url_token=?")

  val SelectShareAndProjectByUrl = query[ShareUrlToken, (Share, Project)](s"""
    SELECT ${share_* inTable "s"}, ${project_* inTable "p"}
    FROM share s, project p
    WHERE url_token=? AND s.project_id = p.id AND $projectIsLive
    """.sql)

  val SummariseShares = query[ProjectId, ShareSummary](
    "SELECT id, url_token, name, uc_filter, view_count, to_iso8601_str(last_viewed_at)" +
      " FROM share WHERE project_id=? ORDER by name")
}


// #####################################################################################################################
// Diagnostics & Stats
private[db] final object AdminSql {

  val DiagSelectNow = queryNA[DateTime]("select now()")

  val StatsCountUsers = queryNA[(Long, Long)]("select count(username), count(1) from usr")

  val StatsDatabaseSize = query[String, Long]("SELECT pg_database_size(?)")

  val StatsSizesByTypes = query[String, (String, Long)]("""
    WITH a as (
        SELECT
          relname "name"
          ,pg_total_relation_size(C.oid) "size"
        FROM pg_class C
        LEFT JOIN pg_namespace N ON (N.oid = C.relnamespace)
        WHERE nspname NOT IN ('pg_catalog', 'information_schema')
         AND nspname !~ '^pg_toast'
         AND C.relkind = ?
      ), b AS (
        SELECT * FROM a WHERE size != 0
        UNION SELECT '*', sum(size) FROM a
      )
      SELECT * FROM b WHERE size != 0 ORDER BY 1;
    """.sql)

}
