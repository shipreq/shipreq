package shipreq.webapp.feature

import scalaz.Validation
import shipreq.webapp.lib.Types._

/*
 * CorrectionPart[-I, +C]
 *   CI                : C @@ InputCorrected
 *   correct           : I  => CI
 *
 * ValidationPart[-C, +V]
 *   validate          : CI => ValidationResult[V]
 *
 * Validator[-I, C, +V]
 *   correctAndValidate: I  => ValidationResult[V]
 *   isValid           : CI => Boolean
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
 * ConstraintValidator[T]
 *   fieldName  : String
 *   constraints: List[Constraint[T]]
 *   validate   : T @@ InputCorrected => ValidationResultT[T]
 *
 * Constraint[-T]
 *   apply  : T => Option[String]
 *   isValid: T => Boolean
 */
package object validation {

  type ValidationResult[+A] = Validation[VFailure, A]

  type ValidationResultT[+A <: AnyRef] = ValidationResult[A @@ Validated]

  object ValidationResult {
    def apply[A](value: A): ValidationResult[A] = Validation.success(value)
  }
}
