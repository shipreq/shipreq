package shipreq.webapp.feature

import scalaz.Validation

package object validation {

  type ValidationResult[+A] = Validation[VFailure, A]

  object ValidationResult {
    def apply[A](value: A): ValidationResult[A] = Validation.success(value)
  }
}
