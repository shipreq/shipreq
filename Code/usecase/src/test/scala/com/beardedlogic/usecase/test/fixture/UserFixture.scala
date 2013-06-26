package com.beardedlogic.usecase
package test.fixture

import scala.slick.jdbc.{StaticQuery => Q}
import scala.slick.session.Session
import lib.db.DB
import lib.security.Oshiro
import test.TestDatabaseSupport

trait UserFixture {

  case class TestUser(username: String, email: String, password: String) {
    val (hashedPassword, salt) = Oshiro.hashWithRandomSalt(password)
  }

  case class PendingTestUser(email: String, token: String)

  val user1 = TestUser("golly", "g@g.com", "hello1234")
  val user2 = TestUser("deepti", "d@d.com", "harvest321")
  val users = List(user1, user2)

  val pendingUser1 = PendingTestUser("a@a.com", "12345678901234567890")
  val pendingUsers = List(pendingUser1)

  def initUserFixture() {
    TestDatabaseSupport.init()
    DB.withInstance(true)(initUserFixture(_))
  }

  def initUserFixture(db: Session) {
    // Insert mock users (registered)
    val i1 = Q.update[(String, String, String, String)]("INSERT INTO usr(username, email, password, password_salt, password_changed_at, confirmation_sent_at, confirmed_at) VALUES(?,?,?,?,NOW(),NOW(),NOW())")
    users.foreach(u => i1.execute(u.username, u.email, u.hashedPassword, u.salt)(db))

    // Insert mock users (pending confirmation)
    val i2 = Q.update[(String, String)]("INSERT INTO usr(email, confirmation_token, confirmation_sent_at) VALUES(?,?,NOW())")
    pendingUsers.foreach(u => i2.execute(u.email, u.token)(db))
  }
}