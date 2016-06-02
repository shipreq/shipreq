package shipreq.webapp.server.test

import java.sql.Timestamp
import net.liftweb.util.Helpers._
import org.joda.time.DateTime
import scala.slick.jdbc.JdbcBackend.Session
import scala.slick.jdbc.{StaticQuery => Q}
import shipreq.base.util.ThreadLocalRes
import shipreq.taskman.api.{EmailAddr, UserId}
import shipreq.webapp.base.data._
import shipreq.webapp.server.data._
import shipreq.webapp.server.db.Shim
import shipreq.webapp.server.security.{PasswordAndSalt, Roles}
import shipreq.webapp.server.test.UserFixture._

object UserFixture {

  val Transaction: ThreadLocalRes[UserFixture] =
    TestDb.Transaction
      .xmap(apply(_))(_.session)
      .onLend(_.setup())

  /** This writes to the DB for real without using and rolling back a transaction.
    * This is required because security manager (Shiro) has it's own DB connection.
    */
  val Session: ThreadLocalRes[UserFixture] =
    TestDb.Session
      .xmap(apply(_))(_.session)
      .onLend(_.setup())
      .onReturn(_.teardown())

  def apply(implicit session: Session) =
    new UserFixture

  case class TestUser(username: Username, email: EmailAddr, password: String, roles: Set[String], name: String, newsletter: Boolean) {
    var _id: Option[UserId] = None
    def id: UserId = _id.getOrElse(???)
    val pws = PasswordAndSalt.createWithRandomSalt(password)
    def hashedPassword = pws.hashedPassword
    def salt = pws.salt
    def toUserDescriptor = UserDescriptor(id, username, email, roles)

    def withLoggedIn[A](a: => A): A =
      WebappServerTestUtil.withLoggedIn(username.value, password)(a)
  }

  case class PendingTestUser(email: EmailAddr, token: String, tokenCreatedAt: DateTime)
}

class UserFixture()(implicit val session: Session) {

  def toDbUtil =
    DbUtil(session)

  private implicit def timeSpanToTimestamp(t: DateTime): Timestamp = new Timestamp(t.getMillis)
  private implicit def autoUsername(a: String) = Username(a)
  private implicit def autoEmailAddr(a: String) = EmailAddr(a)

  val user1 = TestUser("golly", "g@g.com", "hello1234", Set(Roles.Admin.name), "User One", true)
  val user2 = TestUser("deepti", "d@d.com", "harvest321", Set.empty, "User Two", false)
  val users = List(user1, user2)

  val userWithCurrentToken = PendingTestUser("a@p.com", "abc123abc123", 5.minutes.ago)
  val userWithExpiredToken = PendingTestUser("b@p.com", "poi098poi098", 4.weeks.ago)
  val pendingUsers = List(userWithCurrentToken, userWithExpiredToken)

  def setup(): Unit = {
    // Insert mock users (registered)
    val i1 = Q.query[(String, String, String, String, Option[String]), Long]("INSERT INTO usr(username, email, password, password_salt, password_changed_at, confirmation_sent_at, confirmed_at, roles) VALUES(?,?,?,?,NOW(),NOW(),NOW(),?) RETURNING id")
    for (u <- users) {
      val id = UserId(i1(u.username.value, u.email.value, u.hashedPassword.value, u.salt, UserDescriptor.roleStr(u.roles)).first)
      u._id = Some(id)
      Shim.InsertUsrd(id, u.name, u.newsletter).execute
    }

    // Insert mock users (pending confirmation)
    pendingUsers.foreach(insert)
  }

  def teardown(): Unit = {
    users foreach deleteUser
    pendingUsers foreach deleteUser
  }

  def insert(user: PendingTestUser): Unit =
    Q.update[(String, String, Timestamp)]("INSERT INTO usr(email, confirmation_token, confirmation_sent_at) VALUES(?,?,?)").
      apply(user.email.value, user.token, user.tokenCreatedAt).execute

  def deleteUser(u: TestUser): Unit = {
    deleteUser(u.id.value)
    u._id = None
  }

  def deleteUser(u: PendingTestUser): Unit =
    deleteUserByEmail(u.email.value)

  def deleteUser(id: Long): Unit = {
    Q.update[Long]("DELETE FROM usrh_name WHERE usr_id = ?").apply(id).execute
    Q.update[Long]("DELETE FROM usrd WHERE usr_id = ?").apply(id).execute
    Q.update[Long]("DELETE FROM usr WHERE id = ?").apply(id).execute
  }

  def deleteUserByEmail(email: String): Unit = {
    val id = Q.query[String, Long]("SELECT id FROM usr WHERE email = ?").apply(email).firstOption
    id foreach deleteUser
  }
}
