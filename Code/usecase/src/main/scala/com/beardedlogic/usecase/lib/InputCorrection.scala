package com.beardedlogic.usecase
package lib

import model._
import Misc._

/**
 * Corrects undesirable user input.
 */
object InputCorrection {

  def correct(uc: UseCase) = uc.copy(title = useCaseTitle(uc.title))

  def email(email: String) = WhitespaceRegex.replaceAllIn(email, "")

  def useCaseTitle(title: String) = {
    var t = normaliseWhitespaceInSingleLineString(title)
    if (t.isEmpty) t = Defaults.Title
    t
  }
}