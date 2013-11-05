package com.beardedlogic.usecase.feature.validation

import scalaz.{NonEmptyList, Success, Failure}
import com.beardedlogic.usecase.lib.Types._

object ConstraintValidator {
  def apply[T <: AnyRef](fieldName: String, constraints: Constraint[T]*) =
    new ConstraintValidator[T](fieldName, constraints.toList)
}

/**
 * Collection of constraints that apply to a subject.
 * @param fieldName The field name. Prepend to validation failure messages.
 */
class ConstraintValidator[T <: AnyRef](fieldName: String, constraints: List[Constraint[T]]) {
  def validate(input: T @@ InputCorrected): ValidationResult[T] = {
    val in: T = input

    (List.empty[String] /: constraints)((acc, c) => c(in) match {
      case None      => acc
      case Some(err) => err :: acc
    }) match {
      case Nil    => Success(in.tag)
      case h :: t => Failure(VFailure.forField(fieldName, NonEmptyList.nel(h, t)))
    }
  }
}
