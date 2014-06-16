package shipreq.webapp
package feature.uc

import scalaz.Lens.{lensg, lensFamilyg}
import lib.Types._
import field.{TextField, StepField, StepFieldValue}
import text.{StepText, FreeText}
import util.LensFns._
import db.UseCaseHeader

object Lenses {

  // Header lenses
  val uchTitleL = lensg[UseCaseHeader, String](h => t => h.copy(title = t), _.title)

  // Text field lenses
  val freeTextTextL = lensFamilyg[FreeText, FreeText, String, (String, UcParsingCtx)](
    _ => input => FreeText.parse(input._1)(input._2),
    _.text)

  // Step field lenses

  val sfvStepTextInstL = KeyedLens[StepFieldValue, LocalStepId, StepText](
    sfv => id => newValue => sfv.copy(textmap = sfv.textmap + (id -> newValue)),
    sfv => id => sfv.textmap(id)
  )

  val stepTextTextL = lensFamilyg[StepText, StepText, String, (String, UcParsingCtx)](
    _ => input => StepText.parse(input._1)(input._2),
    _.text)

  // Use Case lenses

  val ucHeaderL = lensg[UseCase, UseCaseHeader](u => h => u.copy(header = h), _.header)

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
  val ucStepTextInstL = ucStepFieldL >@=@> sfvStepTextInstL
  val ucStepTextTextL = ucStepTextInstL >@=> stepTextTextL
  val ucTextFieldTextL = ucTextFieldL >@=> freeTextTextL
}
