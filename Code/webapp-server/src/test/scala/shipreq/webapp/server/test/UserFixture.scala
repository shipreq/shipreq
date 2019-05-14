package shipreq.webapp.server.test

import doobie.imports._
import java.time.Instant
import scalaz.syntax.bind.ToBindOps
import scalaz.syntax.traverse._
import shipreq.base.db.DoobieHelpers._
import shipreq.base.util.FxModule._
import shipreq.base.test.db.{SingleConnectionXA, Usable}
import shipreq.webapp.base.data._
import shipreq.webapp.base.user._
import shipreq.webapp.server.logic._
import shipreq.webapp.server.security.Roles
import shipreq.webapp.server.test.UserFixture._
import shipreq.webapp.server.test.WebappServerTestUtil._

object UserFixture {

  val Session: Usable[UserFixture] =
    TestDb(inTransaction = false)
      .map(xa => Fx(apply(xa)))
      .before(_.setup)
      .after(_.teardown)

  val Transaction: Usable[UserFixture] =
    TestDb(inTransaction = true)
      .map(xa => Fx(apply(xa)))
      .before(_.setup)

  final case class TestUser(username  : Username,
                            email     : EmailAddr,
                            password  : PlainTextPassword,
                            roles     : Set[String],
                            name      : PersonName,
                            newsletter: Boolean) {
    var _id: Option[UserId] = None
    def id: UserId = _id.getOrElse(sys error s"UserId unavailable for $this")
    val ps = PrepareEnv.global().security.hashPassword(password).unsafeRun()
    def hashedPassword = ps.passwordHash
    def salt = ps.salt
    def toUserDescriptor = User(id, username, roles)
    def toToken = Security.SessionToken(Some(toUserDescriptor))
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

  def setup: Fx[Unit] = {
    // Insert mock users (registered)
    val inserts1: List[Fx[Unit]] =
      for (u <- users) yield
        (for {
          token <- dbAlgebra.createUserPlaceholder(u.email)
          res <- dbAlgebra.completeUserRegistration(token, u.name, u.username, u.ps, u.newsletter)
          _ <- setRoles(u.email, u.roles)
        } yield res match {
          case DB.UserRegistrationResult.Success(id) => u._id = Some(id)
          case x => sys.error(s"User registration failed: $x")
        }).transact(xa)

    // Insert mock users (pending confirmation)
    val inserts2 = pendingUsers.map(insertPendingTestUser)

    (inserts1 ::: inserts2).sequence_
  }

  def teardown: Fx[Unit] =
    (users.map(deleteTestUser) ::: pendingUsers.map(deletePendingTestUser))
      .sequence_

  def insertPendingTestUser(user: PendingTestUser): Fx[Unit] =
    sql"INSERT INTO usr(email, confirmation_token, confirmation_sent_at) VALUES(${user.email.value}, ${user.token}, ${user.tokenCreatedAt})"
      .update.execute.transact(xa)

  def deleteTestUser(u: TestUser): Fx[Unit] =
    Fx(u._id) flatMap {
      case None => Fx.unit
      case Some(id) => deleteUser(id.value).map(_ => u._id = None)
    }

  def deletePendingTestUser(u: PendingTestUser): Fx[Unit] =
    deleteUserByEmail(u.email.value)

  def deleteUser(id: Long): Fx[Unit] =
    sql"DELETE FROM usrh_name WHERE usr_id = $id".update.run.transact(xa) >>
    sql"DELETE FROM usrd WHERE usr_id = $id".update.run.transact(xa) >>
    sql"DELETE FROM usr WHERE id = $id".update.execute.transact(xa)

  def deleteUserByEmail(email: String): Fx[Unit] = {
    val q: Fx[Option[Long]] = sql"SELECT id FROM usr WHERE email = $email".query[Long].option.transact(xa)
    q.flatMap(_.fold(Fx.unit)(deleteUser))
  }
}
