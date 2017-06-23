package shipreq.webapp.server.db

import doobie.imports._
import java.time.Instant
import org.postgresql.util.PSQLException
import scala.collection.immutable.SortedMap
import scalaz.syntax.applicative._
import scalaz.{-\/, Free, \/-}
import shipreq.base.db.DoobieHelpers._
import shipreq.base.db.SqlHelpers._
import shipreq.taskman.api.{EmailAddr, UserId}
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.{ActiveEvent, Event, EventOrd, VerifiedEvent}
import shipreq.webapp.base.hash.HashRec
import shipreq.webapp.server.data._
import shipreq.webapp.server.logic.{ProjectHeader, ProjectId}
import shipreq.webapp.server.security.PasswordAndSalt
import SqlHelpers._

/**
  * TODO Revise
  *
  * Database interface.
  *
  * Methods follow this pattern:
  *
  * - `find`: Searches for a single row. Returns Option[T].
  * - `findAll`: Searches for a multiple rows. Returns List[T], possibly empty.
  * - `findOrCreate`: Searches for an item and creates it if not found. Returns T.
  * - `summarise`: Retrieves a list of objects that summarise one or more data. Returns List[T].
  *
  * - `create`: Creates a new row. Returns T.
  * - `link`: Creates a link/mapping between existing DB records. Returns unit.
  * - `log`: Records a loggable event. Returns unit.
  * - `sync`: Ensures the DB state of a record matches a provided model, changing the DB if necessary.
  * - `update`: Modifies an existing DB record. Return specialised result.
  * - `delete`: Delete data and usually its dependents. Returns unit.
  *
  * - `perform`: Perform specialised business-logic (as opposed to CRUD-like operations).
  * - `diag`: Diagnostic functions.
  */
object DbLogic {

  // ===================================================================================================================
  object user {

    private def tokenAttempt(tokenFn: () => String)(execute: String => ConnectionIO[_]): ConnectionIO[String] =
      ConnectionIoUnit
        .map(_ => tokenFn())
        .inSafeTransaction
        .retry(16)
        .map(_ getOrElse sys.error("Failed to acquire token."))
        .flatMap(t => execute(t).map(_ => t))

    private[db] val sqlInsertPlaceholder = Update[(EmailAddr, String)](
      "INSERT INTO usr(email, confirmation_token, confirmation_sent_at) VALUES(?,?,NOW())")

    /** Creates an unconfirmed user account. No username, no password until email confirmed. */
    def createPlaceholder(email: EmailAddr, tokenFn: () => String): ConnectionIO[String] =
      tokenAttempt(tokenFn)(token => sqlInsertPlaceholder.toUpdate0(email, token).execute)

    private[db] val sqlUpdateConfirmationToken = Update[(String, UserId)](
      "UPDATE usr SET confirmation_token = ?, confirmation_sent_at = NOW() WHERE id=?")

    def updateConfirmationToken(id: UserId, tokenFn: () => String): ConnectionIO[String] =
      tokenAttempt(tokenFn)(token => sqlUpdateConfirmationToken.toUpdate0(token, id).execute)

    def findDescAndCredentials(usernameOrEmail: String): ConnectionIO[Option[(UserDescriptor, PasswordAndSalt)]] =
      if (usernameOrEmail.indexOf('@') == -1)
        findDescAndCredentialsByUsername(Username(usernameOrEmail))
      else
        findDescAndCredentialsByEmail(EmailAddr(usernameOrEmail))

    private val sqlColsDesc = "id,username,email,roles"
    private val sqlColsPwdAndSalt = "password,password_salt"

    private[db] val sqlSelectDescCredByUsername = Query[Username, UserDescAndPasswordInDb](
      s"SELECT $sqlColsDesc,$sqlColsPwdAndSalt FROM usr WHERE username=?")

    def findDescAndCredentialsByUsername(username: Username): ConnectionIO[Option[(UserDescriptor, PasswordAndSalt)]] =
      sqlSelectDescCredByUsername.toQuery0(username).option.map(_.flatMap(_.resolve))

    private[db] val sqlSelectDescCredByEmail = Query[EmailAddr, UserDescAndPasswordInDb](
      s"SELECT $sqlColsDesc,$sqlColsPwdAndSalt FROM usr WHERE email=? AND password IS NOT NULL")

    def findDescAndCredentialsByEmail(email: EmailAddr): ConnectionIO[Option[(UserDescriptor, PasswordAndSalt)]] =
      sqlSelectDescCredByEmail.toQuery0(email).option.map(_.flatMap(_.resolve))

    private val sqlColsRegistrationInfo = "id,confirmation_token,confirmation_sent_at,confirmed_at"

    private[db] val sqlSelectRegInfo = Query[EmailAddr, UserRegistrationInfo](
      s"SELECT $sqlColsRegistrationInfo FROM usr WHERE email=?")

    def findRegistrationInfo(email: EmailAddr): ConnectionIO[Option[UserRegistrationInfo]] =
      sqlSelectRegInfo.toQuery0(email).option

    private[db] val sqlSelectRegAndResetPwInfo = Query[EmailAddr, (UserRegistrationInfo, ResetPasswordInfo)](
      s"SELECT $sqlColsRegistrationInfo, reset_password_token, reset_password_sent_at FROM usr WHERE email=?")

    def findRegAndResetPwInfo(email: EmailAddr): ConnectionIO[Option[(UserRegistrationInfo, ResetPasswordInfo)]] =
      sqlSelectRegAndResetPwInfo.toQuery0(email).option

    private[db] val sqlSelectConfirmationTokenIssuedDate = Query[String, Option[Instant]](
      "SELECT confirmation_sent_at FROM usr WHERE confirmation_token=?")

    def findConfirmationTokenIssuedDate(token: String): ConnectionIO[Option[Instant]] =
      sqlSelectConfirmationTokenIssuedDate.toQuery0(token).option.map(_.flatten)

    private[db] val sqlRegisterUser = Query[(Username, PasswordAndSalt, String, String), UserId](
      """
        UPDATE usr SET username = ?
          ,password = ?, password_salt = ?, password_changed_at = NOW()
          ,confirmation_token = NULL, confirmed_at = NOW()
          ,login_count = 1, last_login_at = NOW(), last_login_ip = ?
        WHERE confirmation_token = ?
        RETURNING id""".sql)

    val sqlInsertUsrd = Update[(UserId, String, Boolean)](
      "INSERT INTO usrd VALUES(?,?,?)")

    def performRegistration(token: String)
                           (username: Username, ps: PasswordAndSalt, ipAddr: String)
                           (name: String, newsletter: Boolean): ConnectionIO[UserRegistrationResult] = {

      import UserRegistrationResult._
      val plan: ConnectionIO[UserRegistrationResult] =
        sqlRegisterUser.toQuery0(username, ps, ipAddr, token).option.attemptSql flatMap {
          case \/-(Some(id)) => sqlInsertUsrd.toUpdate0(id, name, newsletter).run.map(_ => DbSuccess(id))
          case \/-(None) => Free pure NoMatchingConfToken
          case -\/(e: PSQLException) if e.getMessage.contains("usr_username_key") => Free pure UsernameTaken
          case -\/(e) => throw e
        }
      plan.inTransaction
    }

    private[db] val sqlInsertLogin = Update[(UserId, Option[String])](
      "INSERT INTO usr_login_log(usr_id,ip) VALUES(?,?)")

    def logLogin(id: UserId, ip: Option[String]): ConnectionIO[Unit] =
      sqlInsertLogin.toUpdate0(id, ip).execute

    private[db] val sqlUpdatePassword = Update[(PasswordAndSalt, UserId)](
      "UPDATE usr SET password = ?, password_salt = ?, password_changed_at = NOW() WHERE id=?")

    def updatePassword(id: UserId, ps: PasswordAndSalt): ConnectionIO[Unit] =
      sqlUpdatePassword.toUpdate0(ps, id).execute

    private val InstallNewResetPasswordToken = Update[(String, UserId)](
      "UPDATE usr SET reset_password_token = ?, reset_password_sent_at = NOW(), reset_password_req_count = reset_password_req_count + 1 WHERE id=?")

    def performInstallNewResetPasswordToken(u: UserId, tokenFn: () => String): ConnectionIO[String] =
      tokenAttempt(tokenFn)(token => InstallNewResetPasswordToken.toUpdate0(token, u).execute)

    private[db] val sqlReuseResetPasswordToken = Update[UserId](
      "UPDATE usr SET reset_password_sent_at = NOW(), reset_password_req_count = reset_password_req_count + 1 WHERE id=?")

    def performReuseResetPasswordToken(u: UserId): ConnectionIO[Unit] =
      sqlReuseResetPasswordToken.toUpdate0(u).execute

    private[db] val sqlSelectResetPasswordTokenIssuedDate = Query[String, Option[Instant]](
      "SELECT reset_password_sent_at FROM usr WHERE reset_password_token=?")

    def findResetPasswordTokenIssuedDate(token: String): ConnectionIO[Option[Instant]] =
      sqlSelectResetPasswordTokenIssuedDate.toQuery0(token).option.map(_.flatten)

    private[db] val sqlResetPassword = Update[(PasswordAndSalt, String)]( """
      UPDATE usr SET
        password = ?, password_salt = ?, password_changed_at = NOW(),
        reset_password_token = NULL
      WHERE reset_password_token = ? """.sql)

    def performPasswordReset(ps: PasswordAndSalt, token: String): ConnectionIO[Unit] =
      sqlResetPassword.toUpdate0(ps, token).execute
  }

  // ===================================================================================================================
  object project {

    private[db] val sqlCreate = Query[UserId, ProjectId](
      "INSERT INTO project(usr_id) VALUES(?) RETURNING id")

    def create(usrId: UserId): ConnectionIO[ProjectId] =
      sqlCreate.toQuery0(usrId).unique

    private[db] val sqlSelectOwner = Query[ProjectId, UserId](
      "SELECT usr_id FROM project WHERE id=?")

    def findOwner(id: ProjectId): ConnectionIO[Option[UserId]] =
      sqlSelectOwner.toQuery0(id).option

    import shipreq.webapp.base.event._

    private def eventTypeId(e: ActiveEvent): Short =
      EventDbCodecs.eventCodecRegistry.writer(e)._1

    private val projectNameSetId: Short =
      eventTypeId(ProjectNameSet(null))

    private def sqlProjectMetaData(projectCond: String, extraCols: String = ""): String = {

      val reqCreationTypeIds: List[Short] =
        Event.reqCreationEventSamples.map(eventTypeId)

      val extraColSuffix: String =
        Option(extraCols).filter(_.nonEmpty).fold("")("," + _)

      s"""
        WITH
          ps AS (
            SELECT id, created_at
            $extraColSuffix
            FROM project
            $projectCond
          ),
          es AS (
            SELECT
              project_id,
              count(*) events,
              count(*) FILTER (WHERE type_id IN (${reqCreationTypeIds mkString ","})) reqs,
              max(event.created_at) last_updated_at
            FROM event
            WHERE project_id IN (select id FROM ps) AND ord > 1
            GROUP BY project_id
          ),
          ns AS (
            SELECT DISTINCT ON (project_id)
              project_id,
              (e.data#>>'{}')::varchar "name"
            FROM event e
            WHERE project_id IN (select id FROM ps) AND type_id=$projectNameSetId
            ORDER BY project_id, ord DESC
          )
        SELECT
          ps.id,
          COALESCE(ns.name,''),
          COALESCE(es.events,0),
          COALESCE(es.reqs,0),
          ps.created_at,
          es.last_updated_at
          $extraColSuffix
        FROM ps
        LEFT JOIN es ON id=es.project_id
        LEFT JOIN ns ON id=ns.project_id
      """.sql
    }

    private[db] val sqlSelectAllProjectMetaDataForUser = Query[UserId, ProjectMetaData](
      sqlProjectMetaData("WHERE usr_id=?"))

    def findAllProjectMetaDataForUser(uid: UserId): ConnectionIO[List[ProjectMetaData]] =
      sqlSelectAllProjectMetaDataForUser.toQuery0(uid).list

//    private[db] val sqlSelectProjectMetaDataAndUser = Query[ProjectId, (ProjectMetaData, UserId)](
//      sqlProjectMetaData("WHERE id=?", "usr_id"))
//
//    def findProjectMetaDataAndUser(pid: ProjectId): ConnectionIO[Option[(ProjectMetaData, UserId)]] =
//      sqlSelectProjectMetaDataAndUser.toQuery0(pid).option

    private[db] val sqlSelectProjectMetaData = Query[ProjectId, ProjectMetaData](
      sqlProjectMetaData("WHERE id=?"))

    def findProjectMetaData(pid: ProjectId): ConnectionIO[Option[ProjectMetaData]] =
      sqlSelectProjectMetaData.toQuery0(pid).option

    private[db] val sqlSelectProjectHeader: Query[(ProjectId, ProjectId), ProjectHeader] = {
      val sql =
        s"""
          |SELECT
          |  usr_id,
          |  COALESCE(
          |    (SELECT (e.data#>>'{}')::varchar
          |      FROM event e
          |      WHERE project_id=? AND type_id=$projectNameSetId
          |      ORDER BY ord DESC
          |      LIMIT 1),
          |    '') "name"
          |FROM project
          |WHERE id=?
        """.stripMargin.sql
      Query(sql)
    }

    def findProjectHeader(pid: ProjectId): ConnectionIO[Option[ProjectHeader]] =
      sqlSelectProjectHeader.toQuery0((pid, pid)).option
  }

  // ===================================================================================================================
  object event {
    import EventSqlHelpers._

    // select coalesce(max(ord)+1,1) from event where project_id=?
    val sqlInsert = Update[(ProjectId, EventOrd, ActiveEvent)](
      s"INSERT INTO event(project_id,ord,$eventE) VALUES(?,?,${eventE_?})")

    val sqlInsertHashRecs = Update[(ProjectId, EventOrd, HashRec)](
      s"INSERT INTO event_hash(project_id,ord,$eventHR) VALUES(?,?,${eventHR_?})")

    def create(p: ProjectId, ord: EventOrd, e: ActiveEvent, hrs: HashRec.Collection): ConnectionIO[Unit] = {
      val addEvent = sqlInsert.toUpdate0(p, ord, e).run
      // TODO Send in bulk instead of foldLeft
      hrs.foldLeft(addEvent)(_ *> sqlInsertHashRecs.toUpdate0(p, ord, _).run).void.inTransaction
    }

    val sqlSelectAll = Query[ProjectId, (EventOrd, Event)](
      s"SELECT ord,$eventE FROM event WHERE project_id=? ORDER BY ord")

    val sqlSelectAllHashes = Query[ProjectId, (EventOrd, HashRec)](
      s"SELECT ord,$eventHR FROM event_hash WHERE project_id=?")

    /** @return Events in order from lowest to highest ord. */
    def findAll(p: ProjectId): ConnectionIO[Vector[(EventOrd, VerifiedEvent)]] = { // TODO
      // TODO DbLogic.event.findAllEvents has shithouse impl
      class Tmp(val e: Event) {
        var hrs = HashRec.emptyCollection
      }
      (for {
        events <- sqlSelectAll.toQuery0(p).list
        hashes <- sqlSelectAllHashes.toQuery0(p).list
      } yield {
        val map = collection.mutable.HashMap.empty[EventOrd, Tmp]
        for (t <- events)
          map.put(t._1, new Tmp(t._2))
        for (t <- hashes)
          map(t._1).hrs += t._2
        val result = Vector.newBuilder[(EventOrd, VerifiedEvent)]
        for ((ord, tmp) <- map)
          result += ((ord, VerifiedEvent(tmp.e, tmp.hrs)))
        result.result().sortBy(_._1.value)
      }).inTransaction
    }

    /** @return Events in order from lowest to highest ord. */
    def findAll2(p: ProjectId): ConnectionIO[SortedMap[EventOrd, VerifiedEvent]] = {
      // TODO DbLogic.event.findAllEvents has shithouse impl
      class Tmp(val e: Event) {
        var hrs = HashRec.emptyCollection
      }
      (for {
        events <- sqlSelectAll.toQuery0(p).list
        hashes <- sqlSelectAllHashes.toQuery0(p).list
      } yield {
        val map = collection.mutable.HashMap.empty[EventOrd, Tmp]
        for (t <- events)
          map.put(t._1, new Tmp(t._2))
        for (t <- hashes)
          map(t._1).hrs += t._2
        val result = SortedMap.newBuilder[EventOrd, VerifiedEvent]
        for ((ord, tmp) <- map)
          result += ((ord, VerifiedEvent(tmp.e, tmp.hrs)))
        result.result()
      }).inTransaction
    }
  }

  // ===================================================================================================================
  object admin {

    val diagSelectNow: ConnectionIO[Instant] =
      Query0[Instant]("select now()").unique

    val statsCountUsers: ConnectionIO[UsrCount] =
      Query0[(Long, Long)]("select count(username), count(1) from usr")
        .unique
        .map((UsrCount.apply _).tupled)

    final case class UsrCount(registered: Long, total: Long) {
      def pendingRegistration = total - registered
    }

    private[db] val sqlStatsSizesByTypes = Query[String, (String, Long)]("""
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

    val statsTableSizes: ConnectionIO[List[(String, Long)]] =
      sqlStatsSizesByTypes.toQuery0("r").list

    val statsIndexSizes: ConnectionIO[List[(String, Long)]] =
      sqlStatsSizesByTypes.toQuery0("i").list

    def statsDatabaseSize(dbName: String): ConnectionIO[Long] =
      Query[String, Long]("SELECT pg_database_size(?)")
        .toQuery0(dbName.replaceFirst("^.*/", ""))
        .unique
  }
}

sealed trait UserRegistrationResult
object UserRegistrationResult {
  case class DbSuccess(userId: UserId) extends UserRegistrationResult
  case object NoMatchingConfToken extends UserRegistrationResult
  case object UsernameTaken extends UserRegistrationResult
}

