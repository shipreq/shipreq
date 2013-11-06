package com.beardedlogic.usecase
package security

import Permission.Ctx

object Permissions {

  val accessProject: Permission = new TypicalPermission {
    override def name = "accessProject"
    override def check(ctx: Ctx) =
      for {
        u <- ctx.user
        p <- ctx.project
      } yield
        u.id == p.owner
  }

  val viewShare = accessProject
}
