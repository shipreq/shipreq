package com.beardedlogic.usecase
package lib

import model._
import Misc._

/**
 * Corrects undesirable user input.
 */
object InputCorrection {

  def correct(uc: UseCase) = uc.copy(title = useCaseTitle(uc.title))

  def email(input: String) = removeAllWhitespace(input)

  def password(input: String) = input

  def username(input: String) = input.trim.toLowerCase

  def usernameOrEmail(input: String) = if (input.indexOf('@') == -1) username(input) else email(input)

  def useCaseTitle(title: String) = {
    var t = normaliseWhitespaceInSingleLineString(title)
    if (t.isEmpty) t = Defaults.Title
    t
  }
}