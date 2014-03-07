package shipreq.webapp
package security

import Permission.Ctx
import app.AppConfig

final object Permissions {

  val accessProject: Permission = new TypicalPermission {
    override def name = "accessProject"
    override def warnOnFailure = true
    override def check(ctx: Ctx) =
      for {
        u <- ctx.user
        p <- ctx.project
      } yield
        cmp(p.owner, u.id, "project.owner == user.id")
  }

  val editShare = accessProject & new TypicalPermission {
    override def name = "editShare"
    override def warnOnFailure = true
    override def check(ctx: Ctx) =
      for {
        p <- ctx.project
        s <- ctx.share
      } yield
        cmp(s.projectId, p.id, "share.projectId == project.id")
  }

  @inline def viewShare = editShare

  val userRegistration: Permission = new TypicalPermission {
    override def name = "userRegistration"
    override def warnOnFailure = false
    override def check(ctx: Ctx) =
      if (AppConfig.AllowRegister())
        True
      else if (ctx.user.exists(_ hasRole Roles.Admin.name))
        True
      else
        False
  }

  val admin = new TypicalPermission {
    def warnOnFailure: Boolean = true
    def name: String = "admin"
    def check(ctx: Ctx): Option[Boolean] =
      ctx.user.map(_ hasRole Roles.Admin.name)
  }
}
