package com.beardedlogic.usecase
package lib

import scalaz.Lens.{lensg, lensFamilyg}
import Types._
import field.{TextField, StepField, StepFieldValue}
import text.{StepText, FreeText}
import util.LensFns._

object Lenses {

  // Header lenses
  val uchTitleL = lensg[UseCaseHeader, String](h => t => h.copy(title = t), _.title)
  val uchNumberL = lensg[UseCaseHeader, Short](h => n => h.copy(number = n), _.number)

  // Text field lenses
  val freeTextTextL = lensFamilyg[FreeText, FreeText, String, (String, StepAndLabelBiMap)](
    _ => input => FreeText.parse(input._1)(input._2),
    _.text)

  // Step field lenses

  val sfvStepTextInstL = KeyedLens[StepFieldValue, LocalStepId, StepText](
    sfv => id => newValue => sfv.copy(textmap = sfv.textmap + (id -> newValue)),
    sfv => id => sfv.textmap(id)
  )

  val stepTextTextL = lensFamilyg[StepText, StepText, String, (String, StepAndLabelBiMap)](
    v => input => StepText.parse(v.stepId, input._1)(input._2),
    _.text)

  // Use Case lenses

  val ucHeaderL = lensg[UseCase, UseCaseHeader](u => h => u.copy(h), _.header)

  val ucTextFieldL = KeyedLens[UseCase, TextField, FreeText](
    uc => f => v => uc.copy(fieldValues = uc.fieldValues + (f ~> v)),
    uc => f => f(uc.fieldValues)
  )

  val ucStepFieldL = KeyedLens[UseCase, StepField, StepFieldValue](
    uc => f => v => uc.copy(fieldValues = uc.fieldValues + (f ~> v)),
    uc => f => f(uc.fieldValues)
  )

  val sfvStepTextTextL = sfvStepTextInstL >@=> stepTextTextL
  val ucTitleL = ucHeaderL >=> uchTitleL
  val ucNumberL = ucHeaderL >=> uchNumberL
  val ucStepTextInstL = ucStepFieldL >@=@> sfvStepTextInstL
  val ucStepTextTextL = ucStepTextInstL >@=> stepTextTextL
  val ucTextFieldTextL = ucTextFieldL >@=> freeTextTextL
}
