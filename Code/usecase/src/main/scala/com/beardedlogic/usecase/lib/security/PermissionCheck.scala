package com.beardedlogic.usecase
package lib.security

import db.UserDescriptor
import lib.DI
import lib.Types._

object PermissionCheck {

  val Denied = new PermissionCheck(None)

  def userCan: PermissionCheck =
    DI.SecurityProvider.vend.loggedInUser match {
      case None => Denied
      case m@Some(_) => new PermissionCheck(m)
    }
}

class PermissionCheck(m: Option[UserDescriptor]) extends Permissions {
  import PermissionCheck._

  def isDenied: Boolean = m.isEmpty
  def isAllowed: Boolean = !isDenied
  def andIfNotThen(f: => Nothing): Unit = if (isDenied) f

  protected final def check(f: UserId => Boolean): PermissionCheck =
    m match {
      case None => this
      case Some(u) => if (f(u)) this else Denied
    }
}
