package com.beardedlogic.usecase
package security

import db.Project

trait Permissions {
  this: PermissionCheck =>

  def readAndUpdate(p: Project) = check(_ == p.owner)(u => s"User #$u is not the owner of $p")
}
