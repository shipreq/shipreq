package com.beardedlogic.usecase
package test.fixture

import scala.slick.jdbc.{StaticQuery => Q}
import scala.slick.session.Session
import lib.db.DB
import lib.security.PasswordAndSalt
import test.TestDatabaseSupport
import com.beardedlogic.usecase.model.UserDescriptor
import net.liftweb.util.Helpers._
import java.sql.Date

trait UserFixture {

  implicit def timeSpanToSqlDate(t: TimeSpan): Date = new Date(t.toDate.getTime)

  case class TestUser(username: String, email: String, password: String) {
    var id = 0L
    val pws = PasswordAndSalt.hashWithRandomSalt(password)
    def hashedPassword = pws.hashedPassword
    def salt = pws.salt
    def toUserDescriptor = UserDescriptor(id, username, email)
  }

  case class PendingTestUser(email: String, token: String, tokenCreatedAt: Date)

  val user1 = TestUser("golly", "g@g.com", "hello1234")
  val user2 = TestUser("deepti", "d@d.com", "harvest321")
  val users = List(user1, user2)

  val userWithCurrentToken = PendingTestUser("a@p.com", "abc123abc123", 5.minutes.ago)
  val userWithExpiredToken = PendingTestUser("b@p.com", "poi098poi098", 4.weeks.ago)
  val pendingUsers = List(userWithCurrentToken, userWithExpiredToken)

  def initUserFixture() {
    TestDatabaseSupport.init()
    DB.withInstance(true)(initUserFixture(_))
  }

  def initUserFixture(db: Session) {
    // Insert mock users (registered)
    val i1 = Q.query[(String, String, String, String), Int]("INSERT INTO usr(username, email, password, password_salt, password_changed_at, confirmation_sent_at, confirmed_at) VALUES(?,?,?,?,NOW(),NOW(),NOW()) RETURNING id")
    for (u <- users) u.id = i1.first(u.username, u.email, u.hashedPassword, u.salt)(db)

    // Insert mock users (pending confirmation)
    pendingUsers.foreach(u => insert(u)(db))
  }

  val insertPending = Q.update[(String, String, Date)]("INSERT INTO usr(email, confirmation_token, confirmation_sent_at) VALUES(?,?,?)")
  def insert(user: PendingTestUser)(implicit db: Session) { insertPending.execute(user.email, user.token, user.tokenCreatedAt) }
}