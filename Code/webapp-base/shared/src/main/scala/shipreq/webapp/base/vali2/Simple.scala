package shipreq.webapp.base.vali2

import japgolly.microlibs.nonempty.NonEmptySet
import scalaz.{-\/, Applicative, Endo, \/, \/-}

object Simple {

  type Invalidity = NonEmptySet[String]

  object Invalidity {
    def apply(s: String): Invalidity =
      NonEmptySet.one(s)

    def toText(i: Invalidity): String =
      i.whole.mkString("\n")

    val applicative: Applicative[Invalidity \/ ?] =
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

  implicit class ScalazEndoExt[A](private val self: Endo[A]) extends AnyVal {
    def correctLive: EndoCorrector[A] = EndoCorrector.live(self.run)
    def correctFull: EndoCorrector[A] = EndoCorrector.full(self.run)
  }

  type InvalidatorLogic[A] = Invalidity => Invalidator[A]

  implicit class SimpleExt_InvalidatorObj(private val ε: Invalidator.type) extends AnyVal {

    def logic[A](f: A => Boolean): InvalidatorLogic[A] =
      err => {
        val someInvalidity = Some(err)
        Invalidator(a => if (f(a)) None else someInvalidity)
      }

    def test[A](t: A => Boolean, i: Invalidity): Invalidator[A] =
      logic(t)(i)

    def testDyn[A](t: A => Boolean, i: A => Invalidity): Invalidator[A] =
      Invalidator(a => if (t(a)) None else Some(i(a)))
  }

//  implicit class SimpleExt_EndoValidator(private val ε: EndoValidator.type) extends AnyVal {
//    def id[A]: EndoValidator[A] =
//      EndoValidator()
//  }
//  implicit class SimpleExt_Validator(private val ε: Validator.type) extends AnyVal {
//    def id[A]: Validator[A, A, A] =
//      EndoValidator.
//  }


  implicit def SimpleExt_Invalidator[A](a: Invalidator[A]): SimpleExt_Invalidator[A] = new SimpleExt_Invalidator(a.invalidate)
  final class SimpleExt_Invalidator[A](private val invalidate: A => Option[Invalidity]) extends AnyVal {
    def toAuditor: Auditor[A, A] =
      Auditor(a => invalidate(a) match {
        case None    => \/-(a)
        case Some(e) => -\/(e)
      })
  }

  final implicit class SimpleExt_EndoValidator[A](private val self: EndoValidator[A]) extends AnyVal {
    def toValidator: Validator[A, A, A] =
      Validator(self.corrector.toCorrector, self.invalidator.toAuditor)
  }

//  implicit def SimpleExt_Auditor[C, V](a: Auditor[C, V]): SimpleExt_Auditor[C, V] = new SimpleExt_Auditor(a.audit)
//  final class SimpleExt_Auditor[C, V](private val audit: C => Invalidity \/ V) extends AnyVal {
//  }

  final implicit class SimpleExt_Validator[I, C, V](private val self: Validator[I, C, V]) extends AnyVal {
    def forField(name: String): Composite.Validator[I, C, V] =
      self.mapError(Composite.Invalidity.forField(name, _))

    def named(fieldName: String): Named[I, C, V] =
      Named(self, fieldName)
  }

  // ===================================================================================================================

  import Composite.Stateful

  /** When creating a simple validator, one often wants to give it (and all of its errors) a name so that they can be
    * identified in a composite context (eg. a name validator's errors remain attributed to "name" when composed into a
    * person validator). However there may be cases where one wants to use the validator in an isolated context without
    * composition.
    *
    * This is retains the validator name and provides anonymous access via [[unnamed]] and named access in [[named]].
    */
  final class Named[I, C, V](val unnamed: Simple.Validator[I, C, V], val name: String) {

    def named: Composite.Validator[I, C, V] =
      unnamed.forField(name)

    def map[II, CC, VV](f: Simple.Validator[I, C, V] => Simple.Validator[II, CC, VV]): Named[II, CC, VV] =
      new Named(f(unnamed), name)

    def appendInvalidator(i: Invalidator[V]): Named[I, C, V] =
      map(_.appendInvalidator(i))

    def lift[S]: Stateful[S, I, C, V] =
      new Stateful(unnamed, _ => unnamed, name)

    def stateful[S](addStatefulValidation: (Simple.Validator[I, C, V], S) => Simple.Validator[I, C, V]): Stateful[S, I, C, V] =
      new Stateful(unnamed, addStatefulValidation(unnamed, _), name)
  }

  object Named {
    def apply[I, C, V](simple: Simple.Validator[I, C, V], fieldName: String): Named[I, C, V] =
      new Named(simple, fieldName)
  }
}
