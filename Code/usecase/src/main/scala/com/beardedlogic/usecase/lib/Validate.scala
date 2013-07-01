package com.beardedlogic.usecase
package lib

import java.util.regex.Pattern
import scala.annotation.tailrec
import app.AppConfig._
import model._

/**
 * Validates data.
 */
object Validate {
  import Validations._

  /** (Loosely) validates an email address. */
  final val email = Validator[String]("Email address",
    RegexValidation("^_+@_+?\\._+$".replace("_", "[^&<>]").r, "is invalid.")
  )

  final val password = Validator[String]("Password",
    HasLengthInRange(PasswordLength),
    RequiresAlphaAndNumber
  )

  final def password2(password1: String, password2: String): Option[String] =
    if (password1 == password2) None else Some("Passwords don't match.")

  final val username = Validator[String]("Username",
    HasLengthInRange(UsernameLength),
    ValidCharValidation("a-z0-9_",   "can only contain letters, numbers and underscores."),
    RegexValidation("^[a-z].*".r,    "must start with a letter."),
    RegexValidation(".*[a-z0-9]$".r, "must end with a letter or a number.")
  )
}

// ---------------------------------------------------------------------------------------------------------------------

/**
 * Collection of validations that apply to a subject.
 * @param fieldName The field name. Prepend to validation failure messages.
 */
class Validator[-T](fieldName: String, validations: List[Validation[T]]) {
  def apply(input: T): Option[String] = Validator.findFirstFailure(input, fieldName, validations)
}

object Validator {
  def apply[T](fieldName: String, validations: Validation[T]*) = new Validator[T](fieldName, validations.toList)

  @tailrec def findFirstFailure[T](input: T, fieldName: String, validations: List[Validation[T]]): Option[String] =
    validations match {
      case Nil => None
      case h :: t => h(input) match {
        case Some(err) => Some(s"$fieldName $err")
        case _ => findFirstFailure(input, fieldName, t)
      }
    }
}

// ---------------------------------------------------------------------------------------------------------------------

/** Validation of a single constraint. */
trait Validation[-T] extends (T => Option[String])

private object Validations {
  implicit def RegexToPattern(regex: scala.util.matching.Regex): Pattern = regex.pattern

  case class RegexValidation(regex: Pattern, errmsg: String) extends Validation[String] {
    private val failure = Some(errmsg)
    def apply(input: String) = if (regex.matcher(input).matches()) None else failure
  }

  object ValidCharValidation {
    def apply(charRegex: String, errmsg: String) = RegexValidation(s"^[$charRegex]*$$".r.pattern, errmsg)
  }

  case class HasLengthInRange(range: Range) extends Validation[String] {
    def apply(input: String) = if (range.contains(input.length)) None else failure
    private val failure = Some(s"must be between ${range.min} and ${range.max} characters long.")
  }

  object RequiresAlphaAndNumber extends RegexValidation(
    ".*?[A-Za-z].*?[0-9].*|.*?[0-9].*?[A-Za-z].*".r.pattern,
    "must contain at least one letter, and at least one number.")

}