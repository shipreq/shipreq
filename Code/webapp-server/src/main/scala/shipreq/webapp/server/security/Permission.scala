package shipreq.webapp.server.security

import scalaz.{Name, Semigroup}
import net.liftweb.common.Logger
import net.liftweb.http.RequestVar
import shipreq.webapp.server.app.Global
import shipreq.webapp.server.logic.ProjectId
import shipreq.webapp.base.user._
import Permission._

object Permission {

  val SomeTrue = Some(true)
  val SomeFalse = Some(false)

  implicit val permissionSemigroup: Semigroup[Permission] = new Semigroup[Permission] {
    override def append(a: Permission, b: => Permission) = a & b
  }

  final case class Ctx(user   : Option[User],
                       project: Option[ProjectId.AndOwner])

  sealed trait Pass
  private val SomePass = Some(new Pass{})

  final class Checker(val ctx: Ctx, val permission: Permission) extends Logger {

    def isPass: Boolean =
      permission.failure(ctx) match {
        case None =>
          true
        case Some(failure) =>
          if (permission.warnOnFailure) warn(s"Permission denied: [$failure] $ctx")
          false
      }

    @inline def isFail = !isPass

    def pass: Option[Pass] =
      if (isPass) SomePass else None
  }

  implicit class RequestVarPermExt[T](val r: RequestVar[T]) extends AnyVal {
    def some[V](implicit ev: T <:< Name[V]): Some[V] = Some(r.get.value)
  }
}

trait Permission {
  def failure(ctx: Ctx): Option[String]
  def &(that: Permission): Permission =
    if (this eq that) this else new AndPermission(this, that)

  final def using(user   : Option[User]     = Global.security.loggedInUser(),
                  project: Option[ProjectId.AndOwner] = None) =
    new Checker(Ctx(user, project), this)

  def warnOnFailure: Boolean
}

trait TypicalPermission extends Permission {
  protected val logger = Logger.apply(Permissions.getClass.getCanonicalName.replace("$", "") + "." + name)

  @inline final def True = Permission.SomeTrue
  @inline final def False = Permission.SomeFalse

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

  @specialized
  protected final def cmp[A](a: A, b: A, msg: String): Boolean = {
    val r = a == b
    logger.debug(s"[$msg] = [$a == $b] = $r")
    r
  }

}

final class AndPermission(a: Permission, b: Permission) extends Permission {
  override def failure(ctx: Permission.Ctx) = a.failure(ctx) orElse b.failure(ctx)
  override def warnOnFailure = a.warnOnFailure || b.warnOnFailure
}
