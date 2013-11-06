package com.beardedlogic.usecase
package security

import scalaz.Semigroup
import net.liftweb.common.Logger
import app.DI
import db.{Share, UserDescriptor, Project}
import Permission._

final object Permission {

  implicit val permissionSemigroup: Semigroup[Permission] = new Semigroup[Permission] {
    override def append(a: Permission, b: => Permission) = a & b
  }

  final case class Ctx(
    user: Option[UserDescriptor],
    project: Option[Project],
    share: Option[Share])

  sealed trait Pass
  private val SomePass = Some(new Pass{})

  final class Checker(val ctx: Ctx, val permission: Permission) extends Logger {

    def isPass: Boolean =
      permission.failure(ctx) match {
        case None =>
          true
        case Some(failure) =>
          warn(s"Permission denied: [$failure] $ctx")
          false
      }

    @inline def isFail = !isPass

    def pass: Option[Pass] =
      if (isPass) SomePass else None
  }
}

trait Permission {
  def failure(ctx: Ctx): Option[String]
  def &(that: Permission): Permission =
    if (this eq that) this else new AndPermission(this, that)

  final def using(
    user: Option[UserDescriptor] = DI.SecurityProvider.vend.loggedInUser,
    project: Option[Project] = None,
    share: Option[Share] = None
    ) =
    new Checker(Ctx(user, project, share), this)
}

trait TypicalPermission extends Permission {
  def check(ctx: Ctx): Option[Boolean]

  def name: String
  final val failedCtx   = Some(s"$name: insufficient ctx")
  final val failedCheck = Some(s"$name: failed")
  final override def failure(ctx: Ctx): Option[String] =
    check(ctx) match {
      case Some(true)  => None
      case Some(false) => failedCheck
      case None        => failedCtx
    }
}

final class AndPermission(a: Permission, b: Permission) extends Permission {
  override def failure(ctx: Permission.Ctx) = a.failure(ctx) orElse b.failure(ctx)
}
