package com.beardedlogic.usecase
package security

import scalaz.{\/, -\/, \/-}
import net.liftweb.common.Logger
import db.UserDescriptor
import lib.DI
import lib.Types.UserId

object PermissionCheck {

  private val DeniedNoUser = new PermissionCheck(-\/("User not logged in."))

  def userCan: PermissionCheck =
    DI.SecurityProvider.vend.loggedInUser match {
      case None    => DeniedNoUser
      case Some(u) => new PermissionCheck(\/-(u))
    }
}

class PermissionCheck(s: String \/ UserDescriptor) extends Permissions with Logger {

  def isDenied: Boolean = s.isLeft
  def isAllowed: Boolean = !isDenied

  def andIfNotThen(f: => Nothing): Unit = if (isDenied) f
  def expect(): Boolean =
    s match {
      case -\/(msg) => warn(s"Permission denied: $msg"); false
      case \/-(_)   => true
    }

  protected final def check(f: UserId => Boolean)(failMsg: UserId => String): PermissionCheck =
    s match {
      case -\/(_) => this
      case \/-(u) => if (f(u)) this else new PermissionCheck(-\/(failMsg(u)))
    }
}
