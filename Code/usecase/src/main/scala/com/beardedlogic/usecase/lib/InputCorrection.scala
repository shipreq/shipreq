package com.beardedlogic.usecase
package lib

import Misc._
import db.UseCaseHeader

/**
 * Corrects undesirable user input.
 */
object InputCorrection {

  def correct(uc: UseCaseHeader) = uc.copy(title = useCaseTitle(uc.title))

  def email(input: String) = removeAllWhitespace(input)

  def password(input: String) = input

  def username(input: String) = input.trim.toLowerCase

  def usernameOrEmail(input: String) = if (input.indexOf('@') == -1) username(input) else email(input)

  def projectName(name: String) = normaliseWhitespaceInSingleLineString(name)

  def useCaseTitle(title: String) = {
    var t = normaliseWhitespaceInSingleLineString(title)
    if (t.isEmpty) t = Defaults.title
    t
  }
}