package shipreq.webapp.server.test

import doobie.imports._
import java.time.Instant
import scalaz.effect.IO
import scalaz.syntax.bind.ToBindOps
import scalaz.syntax.traverse._
import shipreq.base.db.DoobieHelpers._
import shipreq.base.test.db.{SingleConnectionXA, Usable}
import shipreq.webapp.base.data._
import shipreq.webapp.base.user._
import shipreq.webapp.server.db.DbLogic
import shipreq.webapp.server.logic._
import shipreq.webapp.server.security.{AppSecurityRealm, Roles}
import shipreq.webapp.server.test.UserFixture._
import shipreq.webapp.server.test.WebappServerTestUtil._

object UserFixture {

  val Session: Usable[UserFixture] =
    TestDb(inTransaction = false)
      .map(xa => IO(apply(xa)))
      .before(_.setup)
      .after(_.teardown)

  val Transaction: Usable[UserFixture] =
    TestDb(inTransaction = true)
      .map(xa => IO(apply(xa)))
      .before(_.setup)

  final case class TestUser(username  : Username,
                            email     : EmailAddr,
                            password  : PlainTextPassword,
                            roles     : Set[String],
                            name      : PersonName,
                            newsletter: Boolean) {
    var _id: Option[UserId] = None
    def id: UserId = _id.getOrElse(sys error s"UserId unavailable for $this")
    val ps = AppSecurityRealm.randomHashFn(password)
    def hashedPassword = ps.passwordHash
    def salt = ps.salt
    def toUserDescriptor = User(id, username, email, roles)

    def withLoggedIn[A](a: => A): A =
      WebappServerTestUtil.withLoggedIn(username, password)(a)
  }

  final case class PendingTestUser(email: EmailAddr, token: String, tokenCreatedAt: Instant)

  def roleStr(roles: Set[String]): Option[String] =
    if (roles.isEmpty)
      None
    else
      Some(roles.mkString(","))
}

final case class UserFixture(xa: SingleConnectionXA) {
  import PrepareEnv.dbAlgebra

  val user1 = TestUser(Username("golly"), EmailAddr("g@g.com"), PlainTextPassword("hello1234"), Set(Roles.Admin.name), PersonName("User One"), true)
  val user2 = TestUser(Username("deepti"), EmailAddr("d@d.com"), PlainTextPassword("harvest321"), Set.empty, PersonName("User Two"), false)
  val users = List(user1, user2)

  val userWithCurrentToken = PendingTestUser(EmailAddr("a@p.com"), "abc123abc123", 5.minutes.ago)
  val userWithExpiredToken = PendingTestUser(EmailAddr("b@p.com"), "poi098poi098", 4.weeks.ago)
  val pendingUsers = List(userWithCurrentToken, userWithExpiredToken)

  private def setRoles(emailAddr: EmailAddr, roles: Set[String]): ConnectionIO[Unit] =
    sql"UPDATE usr set roles=${Option(roles.mkString(",")).filter(_.nonEmpty)} WHERE email=${emailAddr.value}".update.execute.void

  def setup: IO[Unit] = {
    // Insert mock users (registered)
    val inserts1: List[IO[Unit]] =
      for (u <- users) yield
        (for {
          token <- dbAlgebra.createUserPlaceholder(u.email)
          res <- dbAlgebra.completeUserRegistration(token, u.name, u.username, u.ps, u.newsletter, None)
          _ <- setRoles(u.email, u.roles)
        } yield res match {
          case DB.UserRegistrationResult.Success(id) => u._id = Some(id)
          case x => sys.error(s"User registration failed: $x")
        }).transact(xa)

    // Insert mock users (pending confirmation)
    val inserts2 = pendingUsers.map(insertPendingTestUser)

    (inserts1 ::: inserts2).sequence_
  }

  def teardown: IO[Unit] =
    (users.map(deleteTestUser) ::: pendingUsers.map(deletePendingTestUser))
      .sequence_

  def insertPendingTestUser(user: PendingTestUser): IO[Unit] =
    sql"INSERT INTO usr(email, confirmation_token, confirmation_sent_at) VALUES(${user.email.value}, ${user.token}, ${user.tokenCreatedAt})"
      .update.execute.transact(xa)

  def deleteTestUser(u: TestUser): IO[Unit] =
    IO(u._id) flatMap {
      case None => IO.ioUnit
      case Some(id) => deleteUser(id.value).map(_ => u._id = None)
    }

  def deletePendingTestUser(u: PendingTestUser): IO[Unit] =
    deleteUserByEmail(u.email.value)

  def deleteUser(id: Long): IO[Unit] =
    sql"DELETE FROM usrh_name WHERE usr_id = $id".update.run.transact(xa) >>
    sql"DELETE FROM usrd WHERE usr_id = $id".update.run.transact(xa) >>
    sql"DELETE FROM usr WHERE id = $id".update.execute.transact(xa)

  def deleteUserByEmail(email: String): IO[Unit] = {
    val q: IO[Option[Long]] = sql"SELECT id FROM usr WHERE email = $email".query[Long].option.transact(xa)
    q.flatMap(_.fold(IO.ioUnit)(deleteUser))
  }
}
