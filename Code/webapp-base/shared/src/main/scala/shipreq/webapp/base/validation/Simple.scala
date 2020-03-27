package shipreq.webapp.base.validation

import japgolly.microlibs.nonempty.{NonEmptySet, NonEmptyVector}
import scalaz.{Applicative, \/}

object Simple {

  type Invalidity = NonEmptySet[String]

  object Invalidity {
    def apply(s: String): Invalidity =
      NonEmptySet.one(s)

    def toLines(invalidity: Invalidity): NonEmptyVector[String] =
      invalidity.toNEV.sorted

    def toText(invalidity: Invalidity): String =
      toLines(invalidity).whole.mkString("\n")

    // ****[  Don't forget there's: GeneralTheme.renderSimpleInvalidity  ]****

    val applicative: Applicative[Invalidity \/ *] =
      Generic.AccumuateErrors.applicativeInstance[Invalidity]
  }

  type EndoCorrector[A] = Generic.EndoCorrector[A]
  val EndoCorrector = Generic.EndoCorrector

  type Invalidator[A] = Generic.Invalidator[Invalidity, A]
  val Invalidator = Generic.Invalidator

  type EndoValidator[A] = Generic.EndoValidator[Invalidity, A]
  val EndoValidator = Generic.EndoValidator

  type Corrector[I, C] = Generic.Corrector[I, C]
  val Corrector = Generic.Corrector

  type Auditor[C, V] = Generic.Auditor[Invalidity, C, V]
  val Auditor = Generic.Auditor

  type Validator[I, C, V] = Generic.Validator[Invalidity, I, C, V]
  val Validator = Generic.Validator

  type InvalidatorLogic[A] = Invalidity => Invalidator[A]
}
