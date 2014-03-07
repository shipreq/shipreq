package shipreq.webapp
package test.fixture

import java.sql.Timestamp
import net.liftweb.util.Helpers._
import org.joda.time.DateTime
import scala.slick.jdbc.{StaticQuery => Q}
import scala.slick.session.Session

import db.UserDescriptor
import security.{Roles, PasswordAndSalt}
import test.{TestDB, TestHelpers}
import lib.Types._

trait UserFixture {
  this: TestHelpers =>

  implicit def timeSpanToTimestamp(t: DateTime): Timestamp = new Timestamp(t.getMillis)

  case class TestUser(username: String, email: String, password: String, roles: Set[String]) {
    var _id: Option[UserId] = None
    def id: UserId = _id.getOrElse(???)
    val pws = PasswordAndSalt.createWithRandomSalt(password)
    def hashedPassword = pws.hashedPassword
    def salt = pws.salt
    def toUserDescriptor = UserDescriptor(id, username, email, roles)
  }

  case class PendingTestUser(email: String, token: String, tokenCreatedAt: DateTime)

  val user1 = TestUser("golly", "g@g.com", "hello1234", Set(Roles.Admin.name))
  val user2 = TestUser("deepti", "d@d.com", "harvest321", Set.empty)
  val users = List(user1, user2)

  val userWithCurrentToken = PendingTestUser("a@p.com", "abc123abc123", 5.minutes.ago)
  val userWithExpiredToken = PendingTestUser("b@p.com", "poi098poi098", 4.weeks.ago)
  val pendingUsers = List(userWithCurrentToken, userWithExpiredToken)

  def initUserFixtureWithoutTransaction(): Unit = {
    TestDB.withInstance(true)(initUserFixture(_))
  }

  def initUserFixture(implicit db: Session): Unit = {
    // Insert mock users (registered)
    val i1 = Q.query[(String, String, String, String, Option[String]), Long]("INSERT INTO usr(username, email, password, password_salt, password_changed_at, confirmation_sent_at, confirmed_at, roles) VALUES(?,?,?,?,NOW(),NOW(),NOW(),?) RETURNING id")
    for (u <- users) u._id = Some(i1.first(u.username, u.email, u.hashedPassword, u.salt, UserDescriptor.roleStr(u.roles))(db).tag[IsUserId])

    // Insert mock users (pending confirmation)
    pendingUsers.foreach(u => insert(u)(db))
  }

  def deleteUserFixtureWithoutTransaction(): Unit = TestDB.withInstance(false)(deleteUserFixture(_))

  def deleteUserFixture(implicit db: Session): Unit = {
    users foreach deleteUser
    pendingUsers foreach deleteUser
  }

  def insert(user: PendingTestUser)(implicit db: Session): Unit =
    Q.update[(String, String, Timestamp)]("INSERT INTO usr(email, confirmation_token, confirmation_sent_at) VALUES(?,?,?)").
    execute(user.email, user.token, user.tokenCreatedAt)

  def deleteUser(u: TestUser)(implicit db: Session): Unit = { deleteUser(u.id); u._id = None }
  def deleteUser(u: PendingTestUser)(implicit db: Session): Unit = deleteUserByEmail(u.email)
  def deleteUser(id: Long)(implicit db: Session): Unit = Q.update[Long]("DELETE FROM usr WHERE id = ?").execute(id)
  def deleteUserByEmail(email: String)(implicit db: Session): Unit = Q.update[String]("DELETE FROM usr WHERE email = ?").execute(email)

  def login(user: TestUser): Unit = login(user.username, user.password)
}