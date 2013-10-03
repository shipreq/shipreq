package com.beardedlogic.usecase.lib.security

import com.beardedlogic.usecase.db.UserDescriptor

/**
 * Interface that provides the app with security features.
 */
trait SecurityProvider {
  def loggedInUser: Option[UserDescriptor]
}