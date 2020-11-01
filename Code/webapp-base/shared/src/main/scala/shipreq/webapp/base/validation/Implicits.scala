package shipreq.webapp.base.validation

import scalaz.Endo

object Implicits {

    private lazy val blankInvalidity = Simple.Invalidity("")
    implicit class SimpleExt_InvalidatorLogic[A](private val self: Simple.InvalidatorLogic[A]) extends AnyVal {
      def negate: Simple.InvalidatorLogic[A] =
         i => {
           val ii = Some(i)
           val positive = self(blankInvalidity)
           Simple.Invalidator(positive(_) match {
             case None => ii
             case Some(_) => None
           })
         }
    }

    implicit class SimpleExt_ScalazEndo[A](private val self: Endo[A]) extends AnyVal {
      def correctLive: Simple.EndoCorrector[A] = Simple.EndoCorrector.live(self.run)
      def correctFull: Simple.EndoCorrector[A] = Simple.EndoCorrector.full(self.run)
    }

    implicit class SimpleExt_InvalidatorObj(private val ε: Generic.Invalidator.type) extends AnyVal {

      def logic[A](isValid: A => Boolean): Simple.InvalidatorLogic[A] =
        err => {
          val someInvalidity = Some(err)
          Simple.Invalidator(a => if (isValid(a)) None else someInvalidity)
        }

      def test[A](t: A => Boolean, i: Simple.Invalidity): Simple.Invalidator[A] =
        logic(t)(i)

      def testDyn[A](t: A => Boolean, i: A => Simple.Invalidity): Simple.Invalidator[A] =
        Simple.Invalidator(a => if (t(a)) None else Some(i(a)))
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

    final implicit class SimpleExt_Validator[I, C, V](private val self: Simple.Validator[I, C, V]) extends AnyVal {
      def forField(name: String): Composite.Validator[I, C, V] =
        self.mapError(Composite.Invalidity.forField(name, _))

      def loose: Composite.Validator[I, C, V] =
        self.mapError(Composite.Invalidity.loose)

      def named(fieldName: String): Composite.Stateless[I, C, V] =
        Composite.Stateless(self, fieldName)
    }
}
