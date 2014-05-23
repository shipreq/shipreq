package shipreq.webapp.feature

import scalaz.Validation
import shipreq.webapp.lib.Types._

/*
 * CorrectionPart[-I, +C]
 *   correct           : I  => C @@ InputCorrected
 *
 * ValidationPart[-C, +V]
 *   validate          : C @@ InputCorrected => ValidationResult[V]
 *
 * Validator[-I, C, +V]
 *   correctAndValidate: I  => ValidationResult[V]
 *   isValid           : C @@ InputCorrected => Boolean
 *
 * ValidationResult[+A]            = Validation[VFailure, A]
 * ValidationResultT[+A <: AnyRef] = ValidationResult[A @@ Validated]
 *
 * VFailure (Monoid)
 *   looseMsgs    : List[ErrorMsg]
 *   fieldFailures: Map[String, NonEmptyList[ErrorMsg]]
 *   toText       : String
 *   toHtml       : NodeSeq
 *
 * Constraint[-A]
 *   invalidate: A => List[String]
 *   isValid   : A => Boolean
 */
package object validation {

  type ValidationResult[+A] = Validation[VFailure, A]

  type ValidationResultT[+A <: AnyRef] = ValidationResult[A @@ Validated]

  object ValidationResult {
    def apply[A](value: A): ValidationResult[A] = Validation.success(value)
  }
}
