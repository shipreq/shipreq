package shipreq.webapp.server.test

import doobie._
import doobie.implicits._
import java.time.Instant
import shipreq.base.db.BaseDoobieCodecs._
import shipreq.base.db.DoobieHelpers._
import shipreq.base.test.db.{ImperativeXA, TestDb}
import shipreq.base.util.FxModule._
import shipreq.webapp.base.data._
import shipreq.webapp.server.logic.algebra._
import shipreq.webapp.server.test.WebappServerTestUtil._

object UserFixture {

  def use[A](f: UserFixture => A): A =
    TestDb.withImperativeXA { xa =>
      val uf = UserFixture(xa)
      uf.setup()
      f(uf)
    }
}

final case class UserFixture(xa: ImperativeXA) {

  val dbUtil = DbUtil(xa)

  import dbUtil.dbAlgebra

  case class TestUser(username  : Username,
                      email     : EmailAddr,
                      password  : PlainTextPassword,
                      name      : PersonName,
                      newsletter: Boolean) {
    var _id: Option[UserId] = None
    def id: UserId = _id.getOrElse(sys error s"UserId unavailable for $this")
    val ps = dbUtil.security.hashPassword(password).unsafeRun()
    def hashedPassword = ps.passwordHash
    def salt = ps.salt
    def toUserDescriptor = User(id, username)
    def toToken() = Security.SessionToken.anonymous().login(toUserDescriptor)
  }

  case class PendingTestUser(email: EmailAddr, token: String, tokenCreatedAt: Instant)

  val user1 = TestUser(Username("golly"), EmailAddr("g@g.com"), PlainTextPassword("hello1234"), PersonName("User One"), true)
  val user2 = TestUser(Username("deepti"), EmailAddr("d@d.com"), PlainTextPassword("harvest321"), PersonName("User Two"), false)
  val users = List(user1, user2)

  val userWithCurrentToken = PendingTestUser(EmailAddr("a@p.com"), "abc123abc123", 5.minutes.ago)
  val userWithExpiredToken = PendingTestUser(EmailAddr("b@p.com"), "poi098poi098", 4.weeks.ago)
  val pendingUsers = List(userWithCurrentToken, userWithExpiredToken)

  def setup(): Unit = {

    // Insert mock users (registered)
      for (u <- users) {
        val cio: ConnectionIO[Unit] =
          for {
            token <- dbAlgebra.createUserPlaceholder(u.email)
            res   <- dbAlgebra.completeUserRegistration(token, u.name, u.username, u.ps, u.newsletter)
          } yield res match {
            case DB.UserRegistrationResult.Success(id) => u._id = Some(id)
            case x                                     => sys.error(s"User registration failed: $x")
          }
        xa ! cio
      }

    // Insert mock users (pending confirmation)
    pendingUsers.foreach(insertPendingTestUser)
  }

  def teardown(): Unit = {
    users.foreach(deleteTestUser)
    pendingUsers.foreach(deletePendingTestUser)
  }

  def insertPendingTestUser(user: PendingTestUser): Unit =
    xa !
      sql"INSERT INTO usr(email, confirmation_token, confirmation_sent_at) VALUES(${user.email.value}, ${user.token}, ${user.tokenCreatedAt})"
        .update.execute

  def deleteTestUser(u: TestUser): Unit =
    for (id <- u._id) {
      dbUtil.deleteUser(id.value)
      u._id = None
    }

  def deletePendingTestUser(u: PendingTestUser): Unit =
    dbUtil.deleteUserByEmail(u.email.value)
}
