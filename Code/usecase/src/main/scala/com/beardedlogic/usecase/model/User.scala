package com.beardedlogic.usecase
package model

import org.joda.time.DateTime
import scala.slick.jdbc.{StaticQuery => Q, GetResult}
import lib.db.DBHelpers._
import lib.security.PasswordAndSalt
import UserAccessor._

case class UserDescriptor(
  id: Long,
  username: String,
  email: String)

case class UserRegistrationInfo(
  id: Long,
  confirmationToken: Option[String],
  confirmationSentAt: Option[DateTime],
  confirmedAt: Option[DateTime]
  )

/**
 * @since 26/06/2013
 */
object UserAccessor {

  implicit val GetResultUserDescriptor = GetResult(r => UserDescriptor(r.<<, r.<<, r.<<))
  implicit val GetResultPasswordAndSalt = GetResult(r => PasswordAndSalt(r.nextString, r.nextString))
  implicit val GetResultUserRegistrationInfo = GetResult(r => UserRegistrationInfo(r.<<, r.<<, r.<<, r.<<))

  val UserDescCols = "id,username,email"
  val PwdAndSaltCols = "password, password_salt"

  val GetDescAndCredentialsByUsername = Q.query[String, (UserDescriptor, PasswordAndSalt)](s"SELECT $UserDescCols,$PwdAndSaltCols FROM usr WHERE username=?")
  val GetDescAndCredentialsByEmail = Q.query[String, (UserDescriptor, PasswordAndSalt)](s"SELECT $UserDescCols,$PwdAndSaltCols FROM usr WHERE email=? AND password IS NOT NULL")

  val GetRegistrationInfo = Q.query[String, UserRegistrationInfo]("SELECT id, confirmation_token, confirmation_sent_at, confirmed_at FROM usr WHERE email=?")

  val GetConfirmationTokenIssuedDate = Q.query[String, DateTime]("SELECT confirmation_sent_at FROM usr WHERE confirmation_token=?")

  val UpdateConfirmationToken = Q.update[(String, Long)]("UPDATE usr SET confirmation_token = ?, confirmation_sent_at = NOW() WHERE id=?")

  val UpdateOnLogin = Q.update[(String, Long)]("UPDATE usr SET login_count = login_count + 1, last_login_at = NOW(), last_login_ip = ? WHERE id=?")

  val InsertUnconfirmed = Q.update[(String, String)]("INSERT INTO usr(email, confirmation_token, confirmation_sent_at) VALUES(?,?,NOW())")
}

trait UserAccessor extends DatabaseAccessor {

  def findUserDescAndCredentials(usernameOrEmail: String): Option[(UserDescriptor, PasswordAndSalt)] =
    if (usernameOrEmail.indexOf('@') == -1)
      findUserDescAndCredentialsByUsername(usernameOrEmail)
    else
      findUserDescAndCredentialsByEmail(usernameOrEmail)

  def findUserDescAndCredentialsByUsername(username: String) = GetDescAndCredentialsByUsername.firstOption(username)

  def findUserDescAndCredentialsByEmail(email: String) = GetDescAndCredentialsByEmail.firstOption(email)

  def findUserRegistrationInfo(email: String) = GetRegistrationInfo.firstOption(email)

  def findUserConfirmationTokenIssuedDate(token: String) = GetConfirmationTokenIssuedDate.firstOption(token)

  /** Creates an unconfirmed user account. No username, no password until email confirmed. */
  def createUser(email: String, token: String): Unit = InsertUnconfirmed.execute(email, token)

  def updateUserOnLogin(id: Long, ipAddr: String): Unit = UpdateOnLogin.execute(ipAddr, id)

  def updateUserConfirmationToken(id: Long, token: String): Unit = UpdateConfirmationToken.execute(token, id)
}