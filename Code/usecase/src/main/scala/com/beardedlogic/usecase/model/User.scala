package com.beardedlogic.usecase
package model

import scala.slick.jdbc.{StaticQuery => Q, GetResult}
import lib.security.PasswordAndSalt
import UserAccessor._

case class UserDescriptor(
  id: Long,
  username: String,
  email: String)

/**
 * @since 26/06/2013
 */
object UserAccessor {

  implicit val GetResultUserDescriptor = GetResult(r => UserDescriptor(r.<<, r.<<, r.<<))
  implicit val GetResultPasswordAndSalt = GetResult(r => PasswordAndSalt(r.nextString, r.nextString))

  val UserDescCols = "id,username,email"
  val PwdAndSaltCols = "password, password_salt"

  val GetDescAndCredentialsByUsername = Q.query[String, (UserDescriptor, PasswordAndSalt)](s"SELECT $UserDescCols,$PwdAndSaltCols FROM usr WHERE username=?")
  val GetDescAndCredentialsByEmail = Q.query[String, (UserDescriptor, PasswordAndSalt)](s"SELECT $UserDescCols,$PwdAndSaltCols FROM usr WHERE email=? AND password IS NOT NULL")

  val UpdateOnLogin = Q.update[(String, Long)]("UPDATE usr SET login_count = login_count + 1, last_login_at = NOW(), last_login_ip = ? WHERE id=?")
}

trait UserAccessor extends DatabaseAccessor {

  def findUserDescAndCredentials(usernameOrEmail: String): Option[(UserDescriptor, PasswordAndSalt)] =
    if (usernameOrEmail.indexOf('@') == -1)
      findUserDescAndCredentialsByUsername(usernameOrEmail)
    else
      findUserDescAndCredentialsByEmail(usernameOrEmail)

  def findUserDescAndCredentialsByUsername(username: String) = GetDescAndCredentialsByUsername.firstOption(username)

  def findUserDescAndCredentialsByEmail(email: String) = GetDescAndCredentialsByEmail.firstOption(email)

  def updateUserOnLogin(id: Long, ipAddr: String): Unit = UpdateOnLogin.execute(ipAddr, id)
}