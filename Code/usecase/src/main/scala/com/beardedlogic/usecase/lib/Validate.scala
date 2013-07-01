package com.beardedlogic.usecase
package lib

import model._

/**
 * Validates data.
 */
object Validate {

  /** (Loosely) validates an email address. */
  def email(email: String) = ValidEmailRegex.matcher(email).matches
  private final val ValidEmailRegex = "^_+@_+?\\._+$".replace("_", "[^&<>]").r.pattern
}
