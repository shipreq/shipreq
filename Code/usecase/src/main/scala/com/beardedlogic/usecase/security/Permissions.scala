package com.beardedlogic.usecase
package security

import Permission.Ctx

final object Permissions {

  val accessProject: Permission = new TypicalPermission {
    override def name = "accessProject"
    override def check(ctx: Ctx) =
      for {
        u <- ctx.user
        p <- ctx.project
      } yield
        cmp(p.owner, u.id, "project.owner == user.id")
  }

  val editShare = accessProject & new TypicalPermission {
    override def name = "editShare"
    override def check(ctx: Ctx) =
      for {
        p <- ctx.project
        s <- ctx.share
      } yield
        cmp(s.projectId, p.id, "share.projectId == project.id")
  }

  @inline def viewShare = editShare
}
