package com.beardedlogic.usecase
package security

import scalaz.{\/, -\/, \/-}
import net.liftweb.common.Logger
import db.UserDescriptor
import lib.Types.UserId
import app.DI

object PermissionCheck {

  private val DeniedNoUser = new PermissionCheck(-\/("User not logged in."))
  private val SomeUnit = Some(())

  def userCan: PermissionCheck =
    DI.SecurityProvider.vend.loggedInUser match {
      case None    => DeniedNoUser
      case Some(u) => new PermissionCheck(\/-(u))
    }
}

class PermissionCheck(s: String \/ UserDescriptor) extends Permissions with Logger {
  import PermissionCheck.SomeUnit

  def isDenied: Boolean = s.isLeft
  def isAllowed: Boolean = !isDenied

  def toOption: Option[Unit] =
    if (isDenied) None else SomeUnit

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
