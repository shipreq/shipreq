package shipreq.webapp.base.vali2

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.microlibs.nonempty.NonEmptyVector
import Simple.SimpleExt_Validator

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

  /** When creating a simple validator, one often wants to give it (and all of its errors) a name so that they can be
    * identified in a composite context (eg. a name validator's errors remain attributed to "name" when composed into a
    * person validator). However there may be cases where one wants to use the validator in an isolated context without
    * composition.
    *
    * This is retains the validator name and provides anonymous access via [[unnamed]] and named access in [[named]].
    */
  final class Stateless[I, C, V](val unnamed: Simple.Validator[I, C, V], val name: String) {

    def named: Composite.Validator[I, C, V] =
      unnamed.forField(name)

    def map[II, CC, VV](f: Simple.Validator[I, C, V] => Simple.Validator[II, CC, VV]): Stateless[II, CC, VV] =
      new Stateless(f(unnamed), name)

    def appendInvalidator(i: Simple.Invalidator[V]): Stateless[I, C, V] =
      map(_.appendInvalidator(i))

    def lift[S]: Stateful[S, I, C, V] =
      new Stateful(unnamed, _ => unnamed, name)

    def stateful[S](addStatefulValidation: (Simple.Validator[I, C, V], S) => Simple.Validator[I, C, V]): Stateful[S, I, C, V] =
      new Stateful(unnamed, addStatefulValidation(unnamed, _), name)
  }

  object Stateless {
    def apply[I, C, V](simple: Simple.Validator[I, C, V], fieldName: String): Stateless[I, C, V] =
      new Stateless(simple, fieldName)
  }

  /** When a validator is used in a holistic context (higher-level scope), it may validate additional properties.
    * Similar to [[Stateless]], this provides different levels/layers of the same logical validator.
    *
    * [[stateless]] - Validates something in isolation.
    * [[unnamed]]   - Validates the same subject as above, in a larger context (`S`).
    * [[named]]     - Same as above except that all errors are attributed to the subject's name (e.g. "Given name").
    *
    * Example: username validation.
    * [[stateless]] - Validates format, say 2-20 chars of [a-z].
    * [[unnamed]]   - In addition to above, validates that the username isn't already is use.
    * [[named]]     - As above except errors are keyed by Some("Username").
    */
  final class Stateful[S, I, C, V](val stateless: Simple.Validator[I, C, V],
                                   val unnamedFn: S => Simple.Validator[I, C, V],
                                   val name     : String) {

    def apply(s: S): Stateless[I, C, V] =
      new Stateless(unnamedFn(s), name)

    def contramap[SS](f: SS => S): Stateful[SS, I, C, V] =
      new Stateful(stateless, unnamedFn compose f, name)

    def map[II, CC, VV](f: Simple.Validator[I, C, V] => Simple.Validator[II, CC, VV]): Stateful[S, II, CC, VV] =
      new Stateful(f(stateless), f compose unnamedFn, name)

  }
}
