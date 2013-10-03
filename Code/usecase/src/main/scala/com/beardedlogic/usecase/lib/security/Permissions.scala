package com.beardedlogic.usecase
package lib.security

import db.Project

trait Permissions {
  this: PermissionCheck =>

  def readAndUpdate(p: Project) = check(_ == p.owner)
}
