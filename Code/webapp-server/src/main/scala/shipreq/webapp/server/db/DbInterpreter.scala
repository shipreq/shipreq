package shipreq.webapp.server.db

import doobie.imports._
import japgolly.univeq._
import java.time.Instant
import nyaya.gen.Gen
import org.postgresql.util.PSQLException
import scala.collection.immutable.TreeSet
import scalaz.syntax.applicative._
import scalaz.syntax.catchable._
import scalaz.{-\/, Free, \/, \/-}
import shipreq.base.db.DoobieHelpers._
import shipreq.base.db.SqlHelpers._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.ApplyEvent.LogicVer
import shipreq.webapp.base.event._
import shipreq.webapp.base.hash._
import shipreq.webapp.base.user._
import shipreq.webapp.server.ServerConfig
import shipreq.webapp.server.db.DbInterpreter._
import shipreq.webapp.server.db.SqlHelpers._
import shipreq.webapp.server.logic.DB.EventFilter
import shipreq.webapp.server.logic._

final class DbInterpreter(implicit config: ServerConfig.Security)
    extends DB.Algebra[ConnectionIO]
       with ForPublicSpa
       with ForMembers
       with SaveProjectEvent
       with Base {

  override val tokenGen: () => SecurityToken = {
    val it = Gen.alphaNumeric.samples()
    val size = config.securityTokenLength
    () => {
      val sb = new StringBuilder(size)
      var i = size
      while (i > 0) {
        i -= 1
        sb.append(it.next())
      }
      SecurityToken(sb.result())
    }
  }
}

object DbInterpreter {

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  trait Base extends DB.Base[ConnectionIO] {

    override final def inDbTransaction[A](f: ConnectionIO[A]): ConnectionIO[A] =
      f.inTransaction

    override final def inDbTransaction[A](level: Int, f: ConnectionIO[A]): ConnectionIO[A] =
      f.inTransaction.withTransactionLevel(level)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object ForSecurity extends DB.ForSecurity[ConnectionIO] {
    private final def colsUserAndPasswordInfo = "id,username,roles,password,password_salt"
    private final type UserAndPasswordInfo = (UserId, Option[Username], Option[String], Option[PasswordHash], Option[Salt])
    private final val parseUserAndPasswordInfo: UserAndPasswordInfo => Option[(User, PasswordAndSalt)] = {
      case (id, ou, or, op, os) =>
        for {
          u <- ou
          p <- op
          s <- os
          r = or.fold(Set.empty[String])(_.split(',').toSet)
        } yield (User(id, u, r), PasswordAndSalt(p, s))
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
  object SecurityTokenReadOnly extends SecurityTokenReadOnly
  trait SecurityTokenReadOnly extends DB.SecurityTokenReadOnly[ConnectionIO] {

    private[db] final val getUserRegistrationTokenIssueDateSql =
      Query[SecurityToken, Instant]("SELECT confirmation_sent_at FROM usr WHERE confirmation_token=?")

    override final def getUserRegistrationTokenIssueDate(t: SecurityToken): ConnectionIO[Option[Instant]] =
      getUserRegistrationTokenIssueDateSql.toQuery0(t).option

    private[db] final val getResetPasswordTokenIssueDateSql =
      Query[SecurityToken, Option[Instant]]("SELECT reset_password_sent_at FROM usr WHERE reset_password_token=?")

    override final def getResetPasswordTokenIssueDate(t: SecurityToken): ConnectionIO[Option[Instant]] =
      getResetPasswordTokenIssueDateSql.toQuery0(t).option.map(_.flatten)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  trait ForPublicSpa extends DB.ForPublicSpa[ConnectionIO] with SecurityTokenReadOnly {
    import DB.{PasswordResetState, UserRegistration, UserRegistrationResult}

    protected val tokenGen: () => SecurityToken

    // TODO: TEST tokenAttempt!
    private final def tokenAttempt(execute: SecurityToken => ConnectionIO[_]): ConnectionIO[SecurityToken] =
      ConnectionIoUnit
        .flatMap { _ =>
          val t = tokenGen()
          execute(t).map(_ => t)
        }
        .inSafeTransaction
        .retry(16)
        .map(_ getOrElse sys.error("Failed to acquire token."))

    private final def colsRegInfo = "id,confirmation_token,confirmation_sent_at,confirmed_at"
    private final type RegInfo = (UserId, Option[SecurityToken], Instant, Option[Instant])
    private final val parseRegInfo: RegInfo => DB.UserRegistration = {
      case (id, Some(t), i, _)       => DB.UserRegistration.Pending(id, t, i)
      case (id, _      , _, Some(i)) => DB.UserRegistration.Complete(id, i)
      case _ => ??? // Protected against this case by TABLE CONSTRAINT usr_confirmation_invariants
    }

    private[db] final val getUserRegistrationSql =
      Query[EmailAddr, RegInfo](s"SELECT $colsRegInfo FROM usr WHERE email=?").map(parseRegInfo)

    override final def getUserRegistration(e: EmailAddr): ConnectionIO[Option[UserRegistration]] =
      getUserRegistrationSql.toQuery0(e).option

    private[db] final val createUserPlaceholderSql =
      Update[(EmailAddr, SecurityToken)]("INSERT INTO usr(email, confirmation_token, confirmation_sent_at) VALUES(?,?,NOW())")

    override final def createUserPlaceholder(e: EmailAddr): ConnectionIO[SecurityToken] =
      tokenAttempt(createUserPlaceholderSql.toUpdate0(e, _).execute)

    private[db] final val updateUserRegistrationTokenSql =
      Update[(SecurityToken, UserId)]("UPDATE usr SET confirmation_token = ?, confirmation_sent_at = NOW() WHERE id=?")

    override final def updateUserRegistrationToken(id: UserId): ConnectionIO[SecurityToken] =
      tokenAttempt(updateUserRegistrationTokenSql.toUpdate0(_, id).execute)

    private[db] final val sqlRegisterUser =
      Query[(Username, PasswordAndSalt, SecurityToken), UserId](
        """
          UPDATE usr SET username = ?
            ,password = ?, password_salt = ?, password_changed_at = NOW()
            ,confirmation_token = NULL, confirmed_at = NOW()
          WHERE confirmation_token = ?
          RETURNING id""".sql)

    private[db] final val sqlInsertUsrd =
      Update[(UserId, PersonName, Boolean)]("INSERT INTO usrd VALUES(?,?,?)")

    override final def completeUserRegistration(token     : SecurityToken,
                                                name      : PersonName,
                                                username  : Username,
                                                ps        : PasswordAndSalt,
                                                newsletter: Boolean): ConnectionIO[UserRegistrationResult] = {
      import UserRegistrationResult._
      val plan: ConnectionIO[UserRegistrationResult] =
        sqlRegisterUser.toQuery0(username, ps, token).option.attemptSql flatMap {
          case \/-(Some(id)) =>
            sqlInsertUsrd.toUpdate0(id, name, newsletter).run.map(_ => Success(id))

          case \/-(None) =>
            Free pure TokenNotFound

          case -\/(e: PSQLException) if e.getMessage.contains("usr_username_key") =>
            Free pure UsernameTaken

          case -\/(e) =>
            throw e
        }
      plan.inTransaction
    }

    private final def colsResetPasswordInfo = "reset_password_token,reset_password_sent_at"
    private final type ResetPasswordInfo = (Option[SecurityToken], Option[Instant])
    private final def colsPasswordResetStateInfo = s"$colsRegInfo,$colsResetPasswordInfo"
    private final type PasswordResetStateInfo = (RegInfo, ResetPasswordInfo)
    private final val parsePasswordResetState: PasswordResetStateInfo => PasswordResetState = {
      val f: (UserRegistration, ResetPasswordInfo) => PasswordResetState = {
        case (r: UserRegistration.Complete, (None   , _      )) => PasswordResetState.NoToken(r)
        case (r: UserRegistration.Complete, (Some(t), Some(i))) => PasswordResetState.TokenExists(r, t, i)
        case (r: UserRegistration.Pending, _)                   => PasswordResetState.UserRegistrationPending(r)
        case _ => ??? // Protected against this case by TABLE CONSTRAINT usr_reset_password_meta
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
      Update[(SecurityToken, UserId)]("UPDATE usr SET reset_password_token = ?, reset_password_sent_at = NOW(), reset_password_req_count = reset_password_req_count + 1 WHERE id=?")

    override final def createResetPasswordToken(id: UserId): ConnectionIO[SecurityToken] =
      tokenAttempt(createResetPasswordTokenSql.toUpdate0(_, id).execute)

    private[db] final val updateResetPasswordTokenOnReissueSql =
      Update[UserId]("UPDATE usr SET reset_password_sent_at = NOW(), reset_password_req_count = reset_password_req_count + 1 WHERE id=?")

    /** Updates the sent-count and sent-at attributes of an existing reset-password token. */
    override final def updateResetPasswordTokenOnReissue(id: UserId): ConnectionIO[Unit] =
      updateResetPasswordTokenOnReissueSql.toUpdate0(id).execute

    private[db] final val updateUserPasswordSql =
      Query[(PasswordAndSalt, SecurityToken), UserId]("""
        UPDATE usr SET
          password = ?, password_salt = ?, password_changed_at = NOW(),
          reset_password_token = NULL
        WHERE reset_password_token = ?
        RETURNING id
        """.sql)

    /** This also clears the token */
    override final def updateUserPassword(token: SecurityToken, ps: PasswordAndSalt): ConnectionIO[Option[UserId]] =
      updateUserPasswordSql.toQuery0(ps, token).option
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  trait SaveProjectEvent extends DB.SaveProjectEvent[ConnectionIO] {
    import DB.SaveProjectEventCmd
    import EventSqlHelpers._

    // select coalesce(max(ord)+1,1) from event where project_id=?
    private[db] final val insertEventSql =
      Update[(ProjectId, EventOrd, ActiveEvent)](s"INSERT INTO event(project_id,ord,$eventE) VALUES(?,?,${eventE_?})")

    private[db] final val insertEventHashSql =
      Update[(ProjectId, EventOrd, HashRecRow)](s"INSERT INTO event_hash(project_id,ord,$sqlHashRecRow) VALUES(?,?,${sqlHashRecRow_?})")

    override final def saveProjectEvents(id: ProjectId, cmds: Traversable[SaveProjectEventCmd]) = {
      val addEvents = insertEventSql.executeBatch(
        cmds.toIterator.map(c => (id, c.ord, c.event)))

      val addHashes = insertEventHashSql.executeBatch(
        for {
          cmd                    <- cmds.toIterator
          (scheme, schemeHashes) <- cmd.hashes.iterator
          (scope, hash)          <- schemeHashes.iterator
        } yield (id, cmd.ord, (scope, LogicVer.SoleInstance, scheme, hash)))

      def result: VerifiedEvent.Seq =
        VerifiedEvent.Seq.empty ++ cmds.toIterator.map(c => VerifiedEvent(c.ord, c.event, c.hashes))

      (addEvents *> addHashes).inTransaction.attempt.map(_.map(_ => result))
    }

    override final def saveProjectEvent(id: ProjectId, cmd: SaveProjectEventCmd) =
      saveProjectEvents(id, cmd :: Nil).map(_.map(_.head))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  trait ForMembers extends DB.ForHomeSpa[ConnectionIO] with DB.ForProjectSpa[ConnectionIO] {
    import EventSqlHelpers._

    private[db] final val createEmptyProjectSql =
      Query[(UserId, Int), ProjectId]("INSERT INTO project(usr_id, init_events) VALUES(?,?) RETURNING id")

    override final def createEmptyProject(id: UserId, initEvents: Int): ConnectionIO[ProjectId] =
       createEmptyProjectSql.toQuery0(id, initEvents).unique

    private final def sqlProjectMetaData(projectCond: String, extraCols: String = ""): String = {

      val reqCreationTypeIds: List[Short] =
        Event.reqCreationEventSamples.map(eventTypeId)

      val extraColSuffix: String =
        Option(extraCols).filter(_.nonEmpty).fold("")("," + _)

      s"""
        WITH
          ps AS (
            SELECT id, init_events, created_at
            $extraColSuffix
            FROM project
            $projectCond
          ),
          es AS (
            SELECT
              project_id,
              max(event.ord) events,
              count(*) FILTER (WHERE type_id IN (${reqCreationTypeIds mkString ","})) reqs,
              max(event.created_at) last_updated_at
            FROM event
            WHERE project_id IN (select id FROM ps)
            GROUP BY project_id
          ),
          ns AS (
            SELECT DISTINCT ON (project_id)
              project_id,
              (e.data#>>'{}')::varchar "name"
            FROM event e
            WHERE project_id IN (select id FROM ps) AND type_id=$eventTypeIdForProjectNameSet
            ORDER BY project_id, ord DESC
          )
        SELECT
          ps.id,
          COALESCE(ns.name,''),
          ps.init_events,
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

    private[db] final val getAllProjectMetaDataForUserSql =
      Query[UserId, ProjectMetaData](sqlProjectMetaData("WHERE usr_id=?"))

    override final def getAllProjectMetaDataForUser(id: UserId): ConnectionIO[List[ProjectMetaData]] =
      getAllProjectMetaDataForUserSql.toQuery0(id).list

    private[db] final val projectSpaInitPageSql: Query[ProjectId, Project.Name] = {
      val sql =
        s"""
           |SELECT (e.data#>>'{}')::varchar
           |FROM event e
           |WHERE project_id=? AND type_id=$eventTypeIdForProjectNameSet
           |ORDER BY ord DESC
           |LIMIT 1
        """.stripMargin.sql
      Query(sql)
    }

    override def projectSpaInitPage(id: ProjectId): ConnectionIO[Project.Name] =
      projectSpaInitPageSql.toQuery0(id).option.map(_.filterNot(_ eq null).getOrElse(""))

    private[db] final val projectSpaInitAppSql: Query[(ProjectId, ProjectId), (Instant, Option[EventOrd.Latest], Option[Instant])] = {
      val sql =
        """
          |SELECT
          |  (SELECT created_at FROM project WHERE id = ?) c,
          |  MAX(ord) l,
          |  MAX(created_at) u
          |FROM event WHERE project_id = ?
        """.stripMargin.sql
      Query(sql)
    }

    override final def projectSpaInitApp(id: ProjectId): ConnectionIO[DB.ProjectSpaInitApp] =
      projectSpaInitAppSql.toQuery0((id, id)).unique.map(x => DB.ProjectSpaInitApp(x._1, x._2, x._3))

    private[db] final val sqlSelectEvents: EventFilter => ProjectId => Query0[(EventOrd, Event)] = {
      type O = (EventOrd, Event)
      val allSql = s"SELECT ord,$eventE FROM event WHERE project_id=?"
      val all = Query[ProjectId, O](allSql)
      val after = Query[(ProjectId,EventOrd), O](s"$allSql AND ord>?")

      {
        case EventFilter.IncludeAll     => all.toQuery0
        case EventFilter.ExcludeUpTo(o) => p => after.toQuery0((p, o))
      }
    }

    private[db] final val sqlSelectAllEventHashes =
      Query[ProjectId, (EventOrd, HashRecRow)](s"SELECT ord,$sqlHashRecRow FROM event_hash WHERE project_id=?")

    private final class TmpForGetAllProjectEvents(val e: Event) {
      private val recsByScheme =
        Array.fill(HashSchemes.schemes.length)(Map.empty: HashRecsForScheme)

      def add(r: HashRecRow): Unit = {
        val i = r._3.id.index
        assert(r._2 ==* LogicVer.SoleInstance)
        val n = recsByScheme(i).updated(r._1, r._4)
        recsByScheme.update(i, n)
      }

      def result(): HashRecs = {
        var hashRecs = HashRecs.empty
        var i = recsByScheme.length
        while (i > 0) {
          i -= 1
          val r = recsByScheme(i)
          if (r.nonEmpty) {
            val id = HashSchemeId(index = i)
            hashRecs = hashRecs.updated(HashSchemes.unsafeGet(id), r)
          }
        }
        hashRecs
      }
    }

    /** @return Events in order from lowest to highest ord. */
    override final def getProjectEvents(p: ProjectId, f: EventFilter): ConnectionIO[VerifiedEvent.Seq] = {
      (for {
        events <- sqlSelectEvents(f)(p).list
        hashes <- sqlSelectAllEventHashes.toQuery0(p).list
      } yield {

        // Create: EventOrd -> (Event, HashRec.Collection)
        // mutable.HashMap.get/put is effectively constant
        // immutable.ListSet.add = linear O(H) where H is hashes per event by (HashScope,LogicVer,HashScheme)
        // time = O(e + h.H)
        val map = collection.mutable.HashMap.empty[EventOrd, TmpForGetAllProjectEvents]
        for (t <- events) map.put(t._1, new TmpForGetAllProjectEvents(t._2))
        for (t <- hashes) map(t._1).add(t._2)

        // time = O(e log e)
        val result = TreeSet.newBuilder[VerifiedEvent]
        for ((ord, tmp) <- map)
          result += VerifiedEvent(ord, tmp.e, tmp.result())
        result.result()

        // TODO Improve getProjectEvents. Currently server time = O(e.log(e) + e + h.H)
        // Ideas: Server (currently) much more constrained in resources than DB.
        // Maybe: have DB sort both queries by ord then traverse both at once | server time ~ O(e.log(e) + h.H)
        // Maybe: do it all in DB and return hash recs as array               | server time ~ O(e.log(e) + h.H)

      }).inTransaction
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  class ForOps(dbName: String) extends DB.ForOps[ConnectionIO] {
    import DB.ForOps._

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
        .list
        .map(_.map((TableStat.apply _).tupled))

    private[db] final val dbSizeSql =
      Query[String, Long]("SELECT pg_database_size(?)")

    override final val dbSize: ConnectionIO[Long] =
      dbSizeSql
        .toQuery0(dbName.replaceFirst("^.*/", ""))
        .unique
  }
}