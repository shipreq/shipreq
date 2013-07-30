package com.beardedlogic.usecase
package test.fixture

import java.sql.Timestamp
import net.liftweb.util.Helpers._
import org.joda.time.DateTime
import scala.slick.jdbc.{StaticQuery => Q}
import scala.slick.session.Session

import lib.db.DB
import lib.security.PasswordAndSalt
import model.UserDescriptor
import test.TestDatabaseSupport

trait UserFixture {

  implicit def timeSpanToTimestamp(t: DateTime): Timestamp = new Timestamp(t.getMillis)

  case class TestUser(username: String, email: String, password: String) {
    var id = 0L
    val pws = PasswordAndSalt.hashWithRandomSalt(password)
    def hashedPassword = pws.hashedPassword
    def salt = pws.salt
    def toUserDescriptor = UserDescriptor(id, username, email)
  }

  case class PendingTestUser(email: String, token: String, tokenCreatedAt: DateTime)

  val user1 = TestUser("golly", "g@g.com", "hello1234")
  val user2 = TestUser("deepti", "d@d.com", "harvest321")
  val users = List(user1, user2)

  val userWithCurrentToken = PendingTestUser("a@p.com", "abc123abc123", 5.minutes.ago)
  val userWithExpiredToken = PendingTestUser("b@p.com", "poi098poi098", 4.weeks.ago)
  val pendingUsers = List(userWithCurrentToken, userWithExpiredToken)

  def initUserFixtureWithoutTransaction(): Unit = {
    TestDatabaseSupport.init()
    DB.withInstance(true)(initUserFixture(_))
  }

  def initUserFixture(implicit db: Session): Unit = {
    // Insert mock users (registered)
    val i1 = Q.query[(String, String, String, String), Int]("INSERT INTO usr(username, email, password, password_salt, password_changed_at, confirmation_sent_at, confirmed_at) VALUES(?,?,?,?,NOW(),NOW(),NOW()) RETURNING id")
    for (u <- users) u.id = i1.first(u.username, u.email, u.hashedPassword, u.salt)(db)

    // Insert mock users (pending confirmation)
    pendingUsers.foreach(u => insert(u)(db))
  }

  def deleteUserFixtureWithoutTransaction(): Unit = DB.withInstance(false)(deleteUserFixture(_))

  def deleteUserFixture(implicit db: Session): Unit = {
    users foreach deleteUser
    pendingUsers foreach deleteUser
  }

  def insert(user: PendingTestUser)(implicit db: Session): Unit =
    Q.update[(String, String, Timestamp)]("INSERT INTO usr(email, confirmation_token, confirmation_sent_at) VALUES(?,?,?)").
    execute(user.email, user.token, user.tokenCreatedAt)

  def deleteUser(u: TestUser)(implicit db: Session): Unit = { deleteUser(u.id); u.id = 0 }
  def deleteUser(u: PendingTestUser)(implicit db: Session): Unit = deleteUserByEmail(u.email)
  def deleteUser(id: Long)(implicit db: Session): Unit = Q.update[Long]("DELETE FROM usr WHERE id = ?").execute(id)
  def deleteUserByEmail(email: String)(implicit db: Session): Unit = Q.update[String]("DELETE FROM usr WHERE email = ?").execute(email)
}