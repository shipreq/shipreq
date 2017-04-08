package shipreq.webapp.base.vali2

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.nonempty.NonEmptyVector

object Composite {

  final case class FieldInvalidity(field: Option[String], reasons: Simple.Invalidity)

  type Invalidity = NonEmptyVector[FieldInvalidity]

  object Invalidity {
    def forField(name: String, reasons: Simple.Invalidity): Invalidity =
      NonEmptyVector.one(FieldInvalidity(Some(name), reasons))

    def toMap(invalidity: Invalidity): Map[Option[String], Simple.Invalidity] =
      invalidity.foldLeft(Map.empty[Option[String], Simple.Invalidity])((m, fi) =>
        m.setOrModifyValue(fi.field, fi.reasons, _ ++ fi.reasons))
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

  // ===================================================================================================================

  /** When a validator is used in a holistic context (higher-level scope), it may validate additional properties.
    * Similar to [[Simple.Named]], this provides different levels/layers of the same logical validator.
    *
    * [[stateless]] - Validates something in isolation.
    * [[simple]]    - Validates the same subject as above, in a larger context (`S`).
    * [[composite]] - Same as above except that all errors are attributed to the subject's name (e.g. "Given name").
    *
    * Example: username validation.
    * [[stateless]] - Validates format, say 2-20 chars of [a-z].
    * [[simple]]    - In addition to above, validates that the username isn't already is use.
    * [[composite]] - As above except errors are keyed by Some("Username").
    */
  final class Stateful[S, I, C, V](val stateless: Simple.Validator[I, C, V],
                                   val simple   : S => Simple.Validator[I, C, V],
                                   val name     : String) {

    import Simple.SimpleExt_Validator

    val composite: S => Composite.Validator[I, C, V] =
      s => simple(s).forField(name)

    def contramap[SS](f: SS => S): Stateful[SS, I, C, V] =
      new Stateful(stateless, simple compose f, name)

    def map[II, CC, VV](f: Simple.Validator[I, C, V] => Simple.Validator[II, CC, VV]): Stateful[S, II, CC, VV] =
      new Stateful(f(stateless), f compose simple, name)

  }
}
