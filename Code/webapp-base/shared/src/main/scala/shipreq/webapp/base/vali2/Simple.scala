package shipreq.webapp.base.vali2

import japgolly.microlibs.nonempty.{NonEmptySet, NonEmptyVector}
import scalaz.{Applicative, Endo, \/}

object Simple {

  type Invalidity = NonEmptySet[String]

  object Invalidity {
    def apply(s: String): Invalidity =
      NonEmptySet.one(s)

    def toLines(invalidity: Invalidity): NonEmptyVector[String] =
      invalidity.toNEV.sorted

    def toText(invalidity: Invalidity): String =
      toLines(invalidity).whole.mkString("\n")

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

  type InvalidatorLogic[A] = Invalidity => Invalidator[A]

  // ===================================================================================================================

  object Implicits {

    private lazy val blankInvalidity = Invalidity("")
    implicit class SimpleExt_InvalidatorLogic[A](private val self: InvalidatorLogic[A]) extends AnyVal {
      def negate: InvalidatorLogic[A] =
         i => {
           val ii = Some(i)
           val positive = self(blankInvalidity)
           Invalidator(positive(_) match {
             case None => ii
             case Some(_) => None
           })
         }
    }

    implicit class SimpleExt_ScalazEndo[A](private val self: Endo[A]) extends AnyVal {
      def correctLive: EndoCorrector[A] = EndoCorrector.live(self.run)
      def correctFull: EndoCorrector[A] = EndoCorrector.full(self.run)
    }

    implicit class SimpleExt_InvalidatorObj(private val ε: Generic.Invalidator.type) extends AnyVal {

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


  //  implicit def SimpleExt_Invalidator[A](a: Invalidator[A]): SimpleExt_Invalidator[A] = new SimpleExt_Invalidator(a.invalidate)
  //  final class SimpleExt_Invalidator[A](private val invalidate: A => Option[Invalidity]) extends AnyVal {
  //  }

  //  final implicit class SimpleExt_EndoValidator[A](private val self: EndoValidator[A]) extends AnyVal {
  //  }

  //  implicit def SimpleExt_Auditor[C, V](a: Auditor[C, V]): SimpleExt_Auditor[C, V] = new SimpleExt_Auditor(a.audit)
  //  final class SimpleExt_Auditor[C, V](private val audit: C => Invalidity \/ V) extends AnyVal {
  //  }

    final implicit class SimpleExt_Validator[I, C, V](private val self: Validator[I, C, V]) extends AnyVal {
      def forField(name: String): Composite.Validator[I, C, V] =
        self.mapError(Composite.Invalidity.forField(name, _))

      def loose: Composite.Validator[I, C, V] =
        self.mapError(Composite.Invalidity.loose)

      def named(fieldName: String): Composite.Stateless[I, C, V] =
        Composite.Stateless(self, fieldName)
    }
  }
}
