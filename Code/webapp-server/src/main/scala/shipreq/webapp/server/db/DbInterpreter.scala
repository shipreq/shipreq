package shipreq.webapp.server.db

import cats.free.Free
import cats.instances.int._
import cats.instances.vector._
import cats.{Monad, ~>}
import doobie._
import doobie.implicits._
import doobie.postgres.circe.jsonb.implicits._
import doobie.postgres.implicits._
import io.circe.Json
import java.time.Instant
import nyaya.gen.Gen
import org.postgresql.util.PSQLException
import scala.collection.immutable.SortedSet
import shipreq.base.db.BaseDoobieCodecs._
import shipreq.base.db.DoobieHelpers._
import shipreq.base.db.SqlHelpers._
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.data._
import shipreq.webapp.member.global.GlobalEvent
import shipreq.webapp.member.project.data.{Live => _, _}
import shipreq.webapp.member.project.event._
import shipreq.webapp.server.db.DbInterpreter._
import shipreq.webapp.server.logic.algebra.DB
import shipreq.webapp.server.logic.algebra.DB.EventFilter
import shipreq.webapp.server.logic.config.ServerLogicConfig
import shipreq.webapp.server.logic.data._
import shipreq.webapp.server.logic.util.Obfuscators

final class DbInterpreter(implicit config: ServerLogicConfig.Security)
    extends DB.Algebra[ConnectionIO]
       with ForPublicSpa
       with ForHomeSpa
       with ForProjectSpa
       with Base {

  override val tokenGen: () => VerificationToken = {
    val it = Gen.alphaNumeric.samples()
    val size = config.verificationTokenLength
    () => {
      val sb = new StringBuilder(size)
      var i = size
      while (i > 0) {
        i -= 1
        sb.append(it.next())
      }
      VerificationToken(sb.result())
    }
  }
}

object DbInterpreter {
  import WebappDoobieCodecs._

  private[db] final val logVisitorStatsSql =
    Query[(ResponseType, Array[String], Int), Unit](s"SELECT visitor_stats_per_hour_add(now(),?,?,?)")

  def logVisitorStats(responseType: ResponseType, uniqueIps: Set[String], requests: Int): ConnectionIO[Unit] =
    logVisitorStatsSql.toQuery0((responseType, uniqueIps.toArray, requests)).unique

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  trait Base extends DB.Base[ConnectionIO] {

    override protected val F = Monad[ConnectionIO]

    override final def withTransactionLevel[F[_], A](runDB: ConnectionIO ~> F, level: Int)(f: ConnectionIO[A]): F[A] =
      runDB(f.withTransactionLevel(level))

    private[db] final val logGlobalEventSql =
      Update[GlobalEventSerialisation.Row]("INSERT INTO global_event(type,data,ip,usr_id) VALUES(?,?,?,?)")

    override def logGlobalEvent(e: GlobalEvent): ConnectionIO[Unit] =
      logGlobalEventSql.toUpdate0(GlobalEventSerialisation.encode(e)).execute

    override def logGlobalEventIf(cond: Boolean)(e: => GlobalEvent): ConnectionIO[Unit] =
      if (cond) logGlobalEvent(e) else ConnectionIoUnit
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object ForSecurity extends DB.ForSecurity[ConnectionIO] {
    private final def colsUserAndPasswordInfo = "id,username,password,password_salt"
    private final type UserAndPasswordInfo = (UserId, Option[Username], Option[PasswordHash], Option[Salt])
    private final val parseUserAndPasswordInfo: UserAndPasswordInfo => Option[(User, PasswordAndSalt)] = {
      case (id, ou, op, os) =>
        for {
          u <- ou
          p <- op
          s <- os
        } yield (User(id, u), PasswordAndSalt(p, s))
    }

    private[db] final val getUserAndPasswordByEmailSql =
      Query[EmailAddr, UserAndPasswordInfo](s"SELECT $colsUserAndPasswordInfo FROM usr WHERE email=? AND password IS NOT NULL")

    override final def getUserAndPasswordByEmail(email: EmailAddr): ConnectionIO[Option[(User, PasswordAndSalt)]] =
      getUserAndPasswordByEmailSql.toQuery0(email).option.map(_.flatMap(parseUserAndPasswordInfo))

    private[db] final val getUserAndPasswordByUsernameSql =
      Query[Username, UserAndPasswordInfo](s"SELECT $colsUserAndPasswordInfo FROM usr WHERE username=?")

    override final def getUserAndPasswordByUsername(username: Username): ConnectionIO[Option[(User, PasswordAndSalt)]] =
      getUserAndPasswordByUsernameSql.toQuery0(username).option.map(_.flatMap(parseUserAndPasswordInfo))

    private[db] final val logLoginSuccessSql =
      Update[(UserId, Option[IP])]("INSERT INTO usr_login_log(usr_id,ip) VALUES(?,?)")

    override final def logLoginSuccess(id: UserId, ip: Option[IP]): ConnectionIO[Unit] =
      logLoginSuccessSql.toUpdate0((id, ip)).execute

    private[db] final val getProjectOwnerSql =
      Query[ProjectId, UserId]("SELECT usr_id FROM project WHERE id=?")

    override final def getProjectOwner(id: ProjectId): ConnectionIO[Option[UserId]] =
      getProjectOwnerSql.toQuery0(id).option
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object VerificationTokenReadOnly extends VerificationTokenReadOnly
  trait VerificationTokenReadOnly extends DB.VerificationTokenReadOnly[ConnectionIO] {

    private[db] final val getUserRegistrationTokenIssueDateSql =
      Query[VerificationToken, Instant]("SELECT confirmation_sent_at FROM usr WHERE confirmation_token=?")

    override final def getUserRegistrationTokenIssueDate(t: VerificationToken): ConnectionIO[Option[Instant]] =
      getUserRegistrationTokenIssueDateSql.toQuery0(t).option

    private[db] final val getResetPasswordTokenIssueDateSql =
      Query[VerificationToken, Option[Instant]]("SELECT reset_password_sent_at FROM usr WHERE reset_password_token=?")

    override final def getResetPasswordTokenIssueDate(t: VerificationToken): ConnectionIO[Option[Instant]] =
      getResetPasswordTokenIssueDateSql.toQuery0(t).option.map(_.flatten)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  trait ForPublicSpa extends DB.ForPublicSpa[ConnectionIO] with VerificationTokenReadOnly {
    import shipreq.webapp.server.logic.algebra.DB.{PasswordResetState, UserRegistration, UserRegistrationResult}

    protected val tokenGen: () => VerificationToken

    protected final lazy val tokenGenCIO: ConnectionIO[VerificationToken] =
      Free.defer(Free.pure(tokenGen()))

    // TODO: TEST tokenAttempt!
    private final def tokenAttempt(execute: VerificationToken => ConnectionIO[_]): ConnectionIO[VerificationToken] =
      tokenGenCIO
        .flatMap(token => execute(token).map(_ => token))
        .inSafeTransaction
        .retry(16)
        .map(_ getOrElse ErrorMsg("Failed to acquire token.").throwException())

    private final def colsRegInfo = "id,confirmation_token,confirmation_sent_at,confirmed_at"
    private final type RegInfo = (UserId, Option[VerificationToken], Instant, Option[Instant])
    private final val parseRegInfo: RegInfo => DB.UserRegistration = {
      case (id, Some(t), i, _)       => DB.UserRegistration.Pending(id, t, i)
      case (id, _      , _, Some(i)) => DB.UserRegistration.Complete(id, i)
      case _                         => throw new IllegalStateException() // Impossible due to CONSTRAINT usr_confirmation_invariants
    }

    private[db] final val getUserIdByEmailSql =
      Query[EmailAddr, UserId](s"SELECT id FROM usr WHERE email=?")

    private[db] final val getUserIdByUsernameSql =
      Query[Username, UserId](s"SELECT id FROM usr WHERE username=?")

    override final def getUserId(e: Username \/ EmailAddr): ConnectionIO[Option[UserId]] =
      e match {
        case -\/(u) => getUserIdByUsernameSql.option(u)
        case \/-(e) => getUserIdByEmailSql.option(e)
      }

    private[db] final val getUserRegistrationSql =
      Query[EmailAddr, RegInfo](s"SELECT $colsRegInfo FROM usr WHERE email=?").map(parseRegInfo)

    override final def getUserRegistration(e: EmailAddr): ConnectionIO[Option[UserRegistration]] =
      getUserRegistrationSql.toQuery0(e).option

    private[db] final val createUserPlaceholderSql =
      Update[(EmailAddr, VerificationToken)]("INSERT INTO usr(email, confirmation_token, confirmation_sent_at) VALUES(?,?,NOW())")

    override final def createUserPlaceholder(e: EmailAddr): ConnectionIO[VerificationToken] =
      tokenAttempt(t => createUserPlaceholderSql.toUpdate0((e, t)).execute)

    private[db] final val updateUserRegistrationTokenSql =
      Update[(VerificationToken, UserId)]("UPDATE usr SET confirmation_token = ?, confirmation_sent_at = NOW() WHERE id=?")

    override final def updateUserRegistrationToken(id: UserId): ConnectionIO[VerificationToken] =
      tokenAttempt(t => updateUserRegistrationTokenSql.toUpdate0((t, id)).execute)

    private[db] final val sqlRegisterUser =
      Query[(Username, PasswordAndSalt, VerificationToken), UserId](
        """
          UPDATE usr SET username = ?
            ,password = ?, password_salt = ?, password_changed_at = NOW()
            ,confirmation_token = NULL, confirmed_at = NOW()
          WHERE confirmation_token = ?
          RETURNING id""".sql)

    private[db] final val sqlInsertUsrd =
      Update[(UserId, PersonName, Boolean, UserEncryptionKey)]("INSERT INTO usrd VALUES(?,?,?,?)")

    override final def completeUserRegistration(token     : VerificationToken,
                                                name      : PersonName,
                                                username  : Username,
                                                ps        : PasswordAndSalt,
                                                newsletter: Boolean,
                                                encKey    : UserEncryptionKey): ConnectionIO[UserRegistrationResult] = {
      import UserRegistrationResult._
      sqlRegisterUser.toQuery0((username, ps, token)).option.attemptSql.flatMap {
        case Right(Some(id)) =>
          sqlInsertUsrd.toUpdate0((id, name, newsletter, encKey)).run.map(_ => Success(id))

        case Right(None) =>
          Free pure TokenNotFound

        case Left(e: PSQLException) if e.getMessage.contains("usr_username_key") =>
          Free pure UsernameTaken

        case Left(e) =>
          throw e
      }
    }

    private final def colsResetPasswordInfo = "reset_password_token,reset_password_sent_at"
    private final type ResetPasswordInfo = (Option[VerificationToken], Option[Instant])
    private final def colsPasswordResetStateInfo = s"$colsRegInfo,$colsResetPasswordInfo"
    private final type PasswordResetStateInfo = (RegInfo, ResetPasswordInfo)
    private final val parsePasswordResetState: PasswordResetStateInfo => PasswordResetState = {
      val f: (UserRegistration, ResetPasswordInfo) => PasswordResetState = {
        case (r: UserRegistration.Complete, (None   , _      )) => PasswordResetState.NoToken(r)
        case (r: UserRegistration.Complete, (Some(t), Some(i))) => PasswordResetState.TokenExists(r, t, i)
        case (r: UserRegistration.Pending, _)                   => PasswordResetState.UserRegistrationPending(r)
        case _                                                  => throw new IllegalStateException() // Impossible due to CONSTRAINT usr_reset_password_meta
      }
      a => f(parseRegInfo(a._1), a._2)
    }

    private[db] final val getPasswordResetStateByEmailSql =
      Query[EmailAddr, PasswordResetStateInfo](s"SELECT $colsPasswordResetStateInfo FROM usr WHERE email=?")
        .map(parsePasswordResetState)

    private final def getPasswordResetStateByEmail(email: EmailAddr): ConnectionIO[Option[DB.PasswordResetState]] =
      getPasswordResetStateByEmailSql.toQuery0(email).option

    private[db] final val getPasswordResetStateByUsernameSql =
      Query[Username, (EmailAddr, PasswordResetStateInfo)](s"SELECT email,$colsPasswordResetStateInfo FROM usr WHERE username=?")
      .map(x => (x._1, parsePasswordResetState(x._2)))

    private final def getPasswordResetStateByUsername(u: Username): ConnectionIO[Option[(EmailAddr, DB.PasswordResetState)]] =
      getPasswordResetStateByUsernameSql.toQuery0(u).option

    override final def getPasswordResetState(ue: Username \/ EmailAddr): ConnectionIO[Option[(EmailAddr, PasswordResetState)]] =
      ue match {
        case \/-(e) => getPasswordResetStateByEmail(e).map(_.map((e, _)))
        case -\/(u) => getPasswordResetStateByUsername(u)
      }

    private[db] final val createResetPasswordTokenSql =
      Update[(VerificationToken, UserId)]("UPDATE usr SET reset_password_token = ?, reset_password_sent_at = NOW(), reset_password_req_count = reset_password_req_count + 1 WHERE id=?")

    override final def createResetPasswordToken(id: UserId): ConnectionIO[VerificationToken] =
      tokenAttempt(t => createResetPasswordTokenSql.toUpdate0((t, id)).execute)

    private[db] final val updateResetPasswordTokenOnReissueSql =
      Update[UserId]("UPDATE usr SET reset_password_sent_at = NOW(), reset_password_req_count = reset_password_req_count + 1 WHERE id=?")

    /** Updates the sent-count and sent-at attributes of an existing reset-password token. */
    override final def updateResetPasswordTokenOnReissue(id: UserId): ConnectionIO[Unit] =
      updateResetPasswordTokenOnReissueSql.toUpdate0(id).execute

    private[db] final val updateUserPasswordSql =
      Query[(PasswordAndSalt, VerificationToken), UserId]("""
        UPDATE usr SET
          password = ?, password_salt = ?, password_changed_at = NOW(),
          reset_password_token = NULL
        WHERE reset_password_token = ?
        RETURNING id
        """.sql)

    /** This also clears the token */
    override final def updateUserPassword(token: VerificationToken, ps: PasswordAndSalt): ConnectionIO[Option[UserId]] =
      updateUserPasswordSql.toQuery0((ps, token)).option
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private def projectMetaDataQuery[A: Write](where: String): Query[A, ProjectMetaData] = {
    type Types = (ProjectId, String, Int, Int, Int, Int, Instant, Instant, Instant)
    val cols = "id, name, events_init, events_total, reqs_live, reqs_total, created_at, accessed_at, updated_at"
    val sql = s"SELECT $cols FROM project WHERE $where"
    Query[A, Types](sql).map {
      case (id, name, events_init, events_total, reqs_live, reqs_total, created_at, accessed_at, updated_at) =>
        ProjectMetaData(
          id            = Obfuscators.projectId.obfuscate(id),
          name          = name,
          eventsInit    = events_init,
          eventsTotal   = events_total,
          reqsLive      = reqs_live,
          reqsTotal     = reqs_total,
          createdAt     = created_at,
          accessedAt    = accessed_at,
          lastUpdatedAt = Option.when(events_total > events_init)(updated_at))
    }
  }

  trait GetProjectMetaData extends DB.GetProjectMetaData[ConnectionIO] {
    private[db] val getProjectMetaDataQuery =
      projectMetaDataQuery[ProjectId]("id=?")

    override def getProjectMetaData(id: ProjectId): ConnectionIO[Option[ProjectMetaData]] =
      getProjectMetaDataQuery.toQuery0(id).option
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  private[db] object GetProjectEventLogic {

    type Row = (EventOrd, Short, Json, Instant)
    type Result = DB.ReadProjectEventError \/ VerifiedEvent.Seq

    def query(where: Fragment): Query0[Row] =
      (fr"SELECT ord,type,data,created_at FROM project_event WHERE " ++ where).query[Row]

    def all(projectId: ProjectId): Query0[Row] =
      query(fr"project_id=$projectId")

    def after(projectId: ProjectId, ord: EventOrd): Query0[Row] =
      query(fr"project_id=$projectId AND ord>$ord")

    def set(projectId: ProjectId, ords: NonEmptySet[EventOrd]): Query0[Row] = {
      val ordsC = cats.data.NonEmptySet(ords.head.value, ords.tail.iterator.map(_.value).to(SortedSet))
      val where = fr"project_id=$projectId AND " ++ Fragments.in(fr"ord", ordsC)
      query(where)
    }
  }

  trait GetProjectEvents extends DB.GetProjectEvents[ConnectionIO] {
    import GetProjectEventLogic._

    override def getProjectEvents(pid: ProjectId, f: EventFilter): ConnectionIO[DB.ReadProjectEventError \/ VerifiedEvent.Seq] = {
      val q: Query0[Row] =
        f match {
          case EventFilter.IncludeAll     => all(pid)
          case EventFilter.ExcludeUpTo(o) => after(pid, o)
          case EventFilter.Set(ords)      => set(pid, ords)
        }

      q.to[Iterator].map { it =>
        var err = Option.empty[DB.ReadProjectEventError]
        var res = VerifiedEvent.Seq.empty

        for (row <- it)
          if (err.isEmpty)
            ProjectEventSerialisation.decode(row._1, row._2, row._3) match {
              case \/-(e) => res += VerifiedEvent(row._1, e, row._4)
              case -\/(e) => err = Some(e)
            }

        err match {
          case None    => \/-(res)
          case Some(e) => -\/(e)
        }
      }
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object SaveProjectEventLogic {

    val insertEventQuery: Query[(ProjectId, EventOrd, Short, Json, UserId), Instant] =
      Query("INSERT INTO project_event (project_id,ord,type,data,usr_id) VALUES(?,?,?,?,?) RETURNING created_at")

    /** unsafe because the ord could be in-use */
    def unsafeInsertEvent(pid: ProjectId, ord: EventOrd, event: ActiveEvent, userId: UserId) = {
      val enc = ProjectEventSerialisation.encode(event)
      insertEventQuery.toQuery0((pid, ord, enc._1, enc._2, userId)).unique
    }

    private def updateProjectSql(moreSets: String) =
      s"UPDATE project SET events_total = events_total + 1, ${moreSets}, accessed_at = now(), updated_at = now() WHERE id=?"

    val updateProjectN: Update[(String, ProjectId)] =
      Update(updateProjectSql("name=?"))

    val updateProjectR: Update[(Int, Int, ProjectId)] =
      Update(updateProjectSql("reqs_live=?, reqs_total=?"))
  }

  trait SaveProjectEvent extends DB.SaveProjectEvent[ConnectionIO] {
    import SaveProjectEventLogic._

    override def saveProjectEvent(pid: ProjectId,
                                  ord: EventOrd,
                                  e  : ActiveEvent,
                                  p  : Project,
                                  uid: UserId): ConnectionIO[DB.SaveProjectEventError \/ VerifiedEvent] = {
      type Result = DB.SaveProjectEventError \/ VerifiedEvent
      unsafeInsertEvent(pid, ord, e, uid).attemptSql.flatMap {
        case Right(now) =>
          val result: Result = \/-(VerifiedEvent(ord, e, now))
          val update: Update0 =
            e match {
              case _: Event.ProjectNameSet => updateProjectN.toUpdate0((p.name, pid))
              case _                       => updateProjectR.toUpdate0((p.liveReqCount, p.content.reqs.size, pid))
            }
          update.run.map(_ => result)

        case Left(e: PSQLException) if e.getMessage.contains("ord_key") =>
          val err: Result = -\/(DB.SaveProjectEventError.OrdInUse)
          Free.pure(err)

        case Left(e) =>
          throw e
      }
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  trait ForHomeSpa extends DB.ForHomeSpa[ConnectionIO] with GetProjectMetaData {

    private[db] val getAllProjectMetaDataForUserQuery =
      projectMetaDataQuery[UserId]("usr_id=?")

    override def getAllProjectMetaDataForUser(id: UserId): ConnectionIO[List[ProjectMetaData]] =
      getAllProjectMetaDataForUserQuery.toQuery0(id).to[List]

    override def createProject(uid: UserId, es: Vector[ActiveEvent], p: Project, k: ProjectEncryptionKey): ConnectionIO[ProjectId] = {
      val events = es.length
      val name   = es.reverseIterator.collectFirst { case e: Event.ProjectNameSet => e.name }.getOrElse("")
      val data   = (uid, events, events, p.liveReqCount, p.content.reqs.size, name, k)
      for {
        pid  <- ForHomeSpa.createProjectQuery.toQuery0(data).unique
        adds = es.iterator.zipWithIndex.map(x => SaveProjectEventLogic.unsafeInsertEvent(pid, EventOrd.fromIndex(x._2), x._1, uid))
        done <- sequentially(adds, pid)
      } yield done
    }
  }

  object ForHomeSpa {

    private[db] val createProjectQuery: Query[(UserId, Int, Int, Int, Int, String, ProjectEncryptionKey), ProjectId] =
      Query(
        """
          |INSERT INTO project(usr_id, events_init, events_total, reqs_live, reqs_total, name, encryption_key)
          |VALUES(?,?,?,?,?,?,?)
          |RETURNING id
        """.stripMargin.sql)
  }

  trait ForProjectSpa
      extends DB.ForProjectSpa[ConnectionIO]
         with GetProjectMetaData
         with GetProjectEvents
         with SaveProjectEvent {

    private val logProjectRead: Update[ProjectId] =
      Update[ProjectId](s"UPDATE project SET accessed_at=now() WHERE id=?")

    private[db] val projectSpaInitPageQuery =
      Query[(ProjectId, UserId), (Project.Name, ProjectEncryptionKey, UserEncryptionKey)](
        "SELECT p.name, p.encryption_key pk, u.encryption_key uk FROM usrd u, project p WHERE p.id=? AND u.usr_id=?")

    override def projectSpaInitPage(pid: ProjectId, uid: UserId): ConnectionIO[Option[DB.ProjectSpaInitPage]] =
      for {
        _ <- logProjectRead.toUpdate0(pid).run
        o <- projectSpaInitPageQuery.toQuery0((pid, uid)).option
      } yield o.map { case (name, pk, uk) => DB.ProjectSpaInitPage(name, uk, pk) }

  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  class ForOps(dbName: String) extends DB.ForOps[ConnectionIO] with GetProjectEvents {
    import shipreq.webapp.server.logic.algebra.DB.ForOps._

    private[db] final val nowSql =
      Query0[Instant]("select now()")

    override final val now: ConnectionIO[Instant] =
      nowSql.unique

    private[db] final val userStatsSql =
      Query0[(Long, Long)]("select count(username), count(1) from usr")

    override final val userStats: ConnectionIO[UserStats] =
      userStatsSql
        .unique
        .map((UserStats.apply _).tupled)

    private[db] final val tableStatsSql =
      Query0[(String, Long, Long)](
        """
          |SELECT
          |    table_name,
          |    pg_table_size(table_name) AS table_size,
          |    pg_indexes_size(table_name) AS indexes_size
          |FROM (
          |    SELECT
          |      (table_schema || '.' || table_name) AS table_name,
          |      table_type
          |    FROM information_schema.tables
          |    WHERE table_schema not like 'pg_%'
          |      AND table_schema <> 'information_schema'
          |) AS all_tables
          |WHERE NOT(table_type = 'VIEW' AND pg_total_relation_size(table_name) = 0)
          |ORDER BY 1
        """.stripMargin.sql)

    override final val tableStats: ConnectionIO[List[TableStat]] =
      tableStatsSql
        .to[List]
        .map(_.map((TableStat.apply _).tupled))

    private[db] final val dbSizeSql =
      Query[String, Long]("SELECT pg_database_size(?)")

    override final val dbSize: ConnectionIO[Long] =
      dbSizeSql
        .toQuery0(dbName.replaceFirst("^.*/", ""))
        .unique

    private[db] val userIdByUsernameSql =
      Query[Username, UserId]("SELECT id FROM usr WHERE username=?")

    private[db] val userIdByEmailSql =
      Query[EmailAddr, UserId]("SELECT id FROM usr WHERE email=?")

    override def getUserId(user: Username \/ EmailAddr): ConnectionIO[Option[UserId]] =
      user match {
        case -\/(u) => userIdByUsernameSql.option(u)
        case \/-(e) => userIdByEmailSql.option(e)
      }

    private[db] val insertVerifiedEventSql: Update[(ProjectId, VerifiedEvent, UserId)] =
      Update[(ProjectId, EventOrd, Short, Json, UserId, Instant)](
        "INSERT INTO project_event (project_id, ord, type, data, usr_id, created_at) VALUES(?,?,?,?,?,?)")
        .contramap[(ProjectId, VerifiedEvent, UserId)] { case (pid, ve, uid) =>
          val (typeId, data) = ProjectEventSerialisation.encodeActiveOrRetired(ve.event)
          (pid, ve.ord, typeId, data, uid, ve.createdAt)
        }

    override def createProject(userId : UserId,
                               events : VerifiedEvent.Seq,
                               project: Project,
                               encKey : ProjectEncryptionKey): ConnectionIO[ProjectId] = {
      val events_init  = 0
      val events_total = events.size
      val reqs_live    = project.content.reqs.reqIterator().count(_.live(project.config.reqTypes) is Live)
      val reqs_total   = project.content.reqs.size
      val name         = project.name
      val creationArgs = (userId, events_init, events_total, reqs_live, reqs_total, name, encKey)
      val eventArgs    = (pid: ProjectId) => events.iterator.map((pid, _, userId)).toVector
      for {
        pid <- ForHomeSpa.createProjectQuery.toQuery0(creationArgs).unique
        _   <- insertVerifiedEventSql.updateMany(eventArgs(pid))
      } yield pid
    }
  }
}