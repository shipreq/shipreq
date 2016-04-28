package shipreq.webapp.server.db

import org.joda.time.DateTime
import scala.slick.jdbc.StaticQuery
import shipreq.base.db.SqlHelpers._
import shipreq.base.db.JodaTimeSqlHelpers._
import shipreq.taskman.api.{EmailAddr, UserId}
import shipreq.webapp.base.event.{ActiveEvent, Event}
import shipreq.webapp.base.hash.HashRec
import shipreq.webapp.server.data._
import shipreq.webapp.server.security.PasswordAndSalt
import StaticQuery.{query, queryNA, update, updateNA}
import SqlHelpers._
import EventSqlHelpers._

/**
 * SQL for all functions exposed in the DAO.
 */
private[db] object Sql {

  private[this] case class Insert() extends scala.annotation.StaticAnnotation
  private[this] case class Update() extends scala.annotation.StaticAnnotation
  private[this] case class Delete() extends scala.annotation.StaticAnnotation

  // ###################################################################################################################
  // User

  private val UserDescCols = "id,username,email,roles"
  private val PwdAndSaltCols = "password,password_salt"
  private val UserRegistrationInfoCols = "id,confirmation_token,confirmation_sent_at,confirmed_at"

  val GetUserDescCredByUsername = query[Username, (UserDescriptor, PasswordAndSalt)](
    s"SELECT $UserDescCols,$PwdAndSaltCols FROM usr WHERE username=?")

  val GetUserDescCredByEmail = query[EmailAddr, (UserDescriptor, PasswordAndSalt)](
    s"SELECT $UserDescCols,$PwdAndSaltCols FROM usr WHERE email=? AND password IS NOT NULL")

  val GetUserRegInfo = query[EmailAddr, UserRegistrationInfo](
    s"SELECT $UserRegistrationInfoCols FROM usr WHERE email=?")

  val GetUserRegAndResetPwInfo = query[EmailAddr, (UserRegistrationInfo, ResetPasswordInfo)](
    s"SELECT $UserRegistrationInfoCols, reset_password_token, reset_password_sent_at FROM usr WHERE email=?")

  val GetConfirmationTokenIssuedDate = query[String, DateTime](
    "SELECT confirmation_sent_at FROM usr WHERE confirmation_token=?")

  @Update val UpdateConfirmationToken = update[(String, UserId)](
    "UPDATE usr SET confirmation_token = ?, confirmation_sent_at = NOW() WHERE id=?")

  @Insert val LogUserLogin = update[(UserId, Option[String])](
    "INSERT INTO usr_login_log(usr_id,ip) VALUES(?,?)")

  @Insert val InsertUserPlaceholder = update[(EmailAddr, String)](
    "INSERT INTO usr(email, confirmation_token, confirmation_sent_at) VALUES(?,?,NOW())")

  @Update val RegisterUser = query[(Username, PasswordAndSalt, String, String), UserId]( """
    UPDATE usr SET username = ?
      ,password = ?, password_salt = ?, password_changed_at = NOW()
      ,confirmation_token = NULL, confirmed_at = NOW()
      ,login_count = 1, last_login_at = NOW(), last_login_ip = ?
    WHERE confirmation_token = ?
    RETURNING id""".sql)

  @Insert val InsertUsrd = update[(UserId, String, Boolean)](
    "INSERT INTO usrd VALUES(?,?,?)")

  private val usrSuppColumns = "password, password_salt, confirmed_at"
  private val usrdColumns = "name, newsletter"
  val GetUserSuppAndDetail = query[UserId, (UserSupplementalInfo, UserDetail)](
    s"SELECT $usrSuppColumns, $usrdColumns FROM usr, usrd WHERE id=? and id=usr_id")

  @Update val UpdateUserDetails = update[(String, Boolean, UserId)](
    "update usrd set name=?, newsletter=? where usr_id=?")

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

  @Update val RenameProject = update[(String, ProjectId, UserId)](
    "UPDATE project SET name=? WHERE id=? AND usr_id=?")

  @Update val DeleteProjectSoft = update[(String, ProjectId)]("UPDATE project SET name=?, deleted_at=NOW() where id=?")
  @Delete val DeleteProjectHard = update[ProjectId](s"DELETE FROM project where id=? and $projectIsDead")

  // ###################################################################################################################
  // Events

  import EventDao.EventSeq

  private final val eventE = "type_id,data_id_type,data_id,data"
  private final val eventE_? = "?,?,?,?"

  private final val eventHR = "scope,logic_ver,hash_scheme,hash"
  private final val eventHR_? = "?,?,?,?"

  // select coalesce(max(seq)+1,1) from event where project_id=?
  @Insert val InsertEvent = update[(ProjectId, EventSeq, ActiveEvent)](
    s"INSERT INTO event(project_id,seq,$eventE) VALUES(?,?,${eventE_?})")

  @Insert val InsertEventHashRecs = update[(ProjectId, EventSeq, HashRec)](
    s"INSERT INTO event_hash(project_id,seq,$eventHR) VALUES(?,?,${eventHR_?})")

  val SelectAllEvents = query[ProjectId, (EventSeq, Event)](
    s"SELECT seq,$eventE FROM event WHERE project_id=? ORDER BY seq")

  val SelectAllEventHashes = query[ProjectId, (EventSeq, HashRec)](
    s"SELECT seq,$eventHR FROM event_hash WHERE project_id=?")
}


// #####################################################################################################################
// Diagnostics & Stats
private[db] object AdminSql {

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
