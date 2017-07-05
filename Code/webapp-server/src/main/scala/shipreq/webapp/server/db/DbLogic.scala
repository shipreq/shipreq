package shipreq.webapp.server.db

import doobie.imports._
import java.time.Instant
import org.postgresql.util.PSQLException
import scala.collection.immutable.SortedMap
import scalaz.syntax.applicative._
import scalaz.{-\/, Free, \/-}
import shipreq.base.db.DoobieHelpers._
import shipreq.base.db.SqlHelpers._
import shipreq.webapp.base.data._
import shipreq.webapp.base.event.{ActiveEvent, Event, EventOrd, VerifiedEvent}
import shipreq.webapp.base.user._
import shipreq.webapp.base.hash.HashRec
import shipreq.webapp.server.logic._
import SqlHelpers._

object DbLogic {

  object user {
    def findDescAndCredentials(usernameOrEmail: String): ConnectionIO[Option[(User, PasswordAndSalt)]] =
      if (EmailAddr.isEmailAddr(usernameOrEmail))
        findDescAndCredentialsByEmail(EmailAddr(usernameOrEmail))
      else
        findDescAndCredentialsByUsername(Username(usernameOrEmail))

    private val sqlColsDesc = "id,username,email,roles"
    private val sqlColsPwdAndSalt = "password,password_salt"

    private case class UserDescAndPasswordInDb(id            : UserId,
                                                   username      : Option[Username],
                                                   email         : EmailAddr,
                                                   rolesStr      : Option[String],
                                                   hashedPassword: Option[PasswordHash],
                                                   salt          : Option[Salt]) {
      def resolve: Option[(User, PasswordAndSalt)] =
        for {
          u <- username
          a <- hashedPassword
          b <- salt
          roles = rolesStr.fold(Set.empty[String])(_.split(',').toSet)
        } yield (User(id, u, email, roles), PasswordAndSalt(a, b))
    }

    private implicit val doobieCompositeUserDescAndPasswordInDb: Composite[UserDescAndPasswordInDb] =
      Composite.generic

    private val sqlSelectDescCredByUsername = Query[Username, UserDescAndPasswordInDb](
      s"SELECT $sqlColsDesc,$sqlColsPwdAndSalt FROM usr WHERE username=?")

    private def findDescAndCredentialsByUsername(username: Username): ConnectionIO[Option[(User, PasswordAndSalt)]] =
      sqlSelectDescCredByUsername.toQuery0(username).option.map(_.flatMap(_.resolve))

    private val sqlSelectDescCredByEmail = Query[EmailAddr, UserDescAndPasswordInDb](
      s"SELECT $sqlColsDesc,$sqlColsPwdAndSalt FROM usr WHERE email=? AND password IS NOT NULL")

    private def findDescAndCredentialsByEmail(email: EmailAddr): ConnectionIO[Option[(User, PasswordAndSalt)]] =
      sqlSelectDescCredByEmail.toQuery0(email).option.map(_.flatMap(_.resolve))
  }

  object project {
    private val sqlSelectOwner = Query[ProjectId, UserId](
      "SELECT usr_id FROM project WHERE id=?")

    def findOwner(id: ProjectId): ConnectionIO[Option[UserId]] =
      sqlSelectOwner.toQuery0(id).option
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
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

    private val sqlStatsSizesByTypes = Query[String, (String, Long)]("""
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
