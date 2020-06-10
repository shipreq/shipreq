package shipreq.webapp.base.validation

import monocle.Iso
import scalaz.Isomorphism.<=>
import scalaz.std.vector.vectorInstance
import scalaz.{-\/, Applicative, Semigroup, Traverse, \/, \/-}
import shipreq.base.util.{GenTuple, Identity, Valid, Validity}

object Generic {

  // ===================================================================================================================
  // Correction
  // ===================================================================================================================

  final case class EndoCorrector[A](live: A => A, full: A => A) {

    def apply(a: A): A =
      full(live(a))

    def appendLive(f: A => A): EndoCorrector[A] =
      EndoCorrector(live andThen f, full)

    def appendFull(f: A => A): EndoCorrector[A] =
      EndoCorrector(live, full andThen f)

    def append(that: EndoCorrector[A]): EndoCorrector[A] =
      EndoCorrector(
        live andThen that.live,
        full andThen that.full)

    def prependLive(f: A => A): EndoCorrector[A] =
      EndoCorrector(live compose f, full)

    def prependFull(f: A => A): EndoCorrector[A] =
      EndoCorrector(live, full compose f)

    def prepend(that: EndoCorrector[A]): EndoCorrector[A] =
      EndoCorrector(
        live compose that.live,
        full compose that.full)

    def xmap[B](g: A => B)(f: B => A): EndoCorrector[B] =
      EndoCorrector(
        g compose live compose f,
        g compose full compose f)

    def imapZ[B](iso: B <=> A): EndoCorrector[B] =
      xmap(iso.from)(iso.to)

    def imap[B](iso: Iso[B, A]): EndoCorrector[B] =
      xmap(iso.reverseGet)(iso.get)

    def pair[AA](implicit A: GenTuple[A, A, AA]): EndoCorrector[AA] =
      this tuple this

    def tuple[A2, AA](b: EndoCorrector[A2])(implicit A: GenTuple[A, A2, AA]): EndoCorrector[AA] =
      EndoCorrector[AA](
        aa => A.map(aa, live, b.live, A.append),
        aa => A.map(aa, full, b.full, A.append))

    def appendValidator[E](c: EndoValidator[E, A]): EndoValidator[E, A] =
      c.prependCorrector(this)

    def toCorrector: Corrector[A, A] =
      Corrector(live, full, Identity[A])

    def toEndoValidator[E]: EndoValidator[E, A] =
      EndoValidator(this, Invalidator.id)

    def withInvalidator[E](i: Invalidator[E, A]): EndoValidator[E, A] =
      EndoValidator(this, i)

    @inline def withAuditor[E, V](v: Auditor[E, A, V]): Validator[E, A, A, V] =
      toCorrector.withAuditor(v)
  }

  object EndoCorrector {
    def id[A]: EndoCorrector[A] =
      EndoCorrector(Identity[A], Identity[A])

    def live[A](f: A => A): EndoCorrector[A] =
      EndoCorrector(f, Identity[A])

    def full[A](f: A => A): EndoCorrector[A] =
      EndoCorrector(Identity[A], f)

    def choose[A](f: A => EndoCorrector[A]): EndoCorrector[A] =
      EndoCorrector(
        a => f(a).live(a),
        a => f(a).full(a))
  }

  // ===================================================================================================================

  final case class Corrector[I, C](live     : I => I,
                                   full     : I => C,
                                   uncorrect: C => I) {

    def apply(i: I): C =
      full(live(i))

    def applyAndUncorrect(i: I): I =
      uncorrect(apply(i))

    def prependLive(f: I => I): Corrector[I, C] =
      copy(live = live compose f)

    def appendLive(f: I => I): Corrector[I, C] =
      copy(live = f compose live)

    def xmapInput[A](g: I => A)(f: A => I): Corrector[A, C] =
      Corrector(
        g compose live compose f,
        full compose f,
        g compose uncorrect)

    def imapInputZ[A](iso: A <=> I): Corrector[A, C] =
      xmapInput(iso.from)(iso.to)

    def imapInput[A](iso: Iso[A, I]): Corrector[A, C] =
      xmapInput(iso.reverseGet)(iso.get)

    def xmapCorrected[A](f: C => A)(g: A => C): Corrector[I, A] =
      Corrector(
        live,
        f compose full,
        uncorrect compose g)

    def imapCorrectedZ[A](iso: C <=> A): Corrector[I, A] =
      xmapCorrected(iso.to)(iso.from)

    def imapCorrected[A](iso: Iso[C, A]): Corrector[I, A] =
      xmapCorrected(iso.get)(iso.reverseGet)

    def pair[II, CC](implicit I: GenTuple[I, I, II], C: GenTuple[C, C, CC]): Corrector[II, CC] =
      this tuple this

    def tuple[I2, C2, II, CC](b: Corrector[I2, C2])(implicit I: GenTuple[I, I2, II], C: GenTuple[C, C2, CC]): Corrector[II, CC] =
      Corrector[II, CC](
        live      = ii => I.map(ii, live, b.live, I.append),
        full      = ii => I.map(ii, full, b.full, C.append),
        uncorrect = cc => C.map(cc, uncorrect, b.uncorrect, I.append))

    def withAuditor[E, V](v: Auditor[E, C, V]): Validator[E, I, C, V] =
      Validator(this, v)

    def vectorWithGaps[G]: Corrector[Vector[G \/ I], Vector[G \/ C]] =
      Corrector[Vector[G \/ I], Vector[G \/ C]](
        live      = _.map(_.map(live)),
        full      = _.map(_.map(full)),
        uncorrect = _.map(_.map(uncorrect)))
  }

  object Corrector {
    def id[A]: Corrector[A, A] =
      Corrector(Identity[A], Identity[A], Identity[A])

    def full[I, C](full: I => C, uncorrect: C => I): Corrector[I, C] =
      Corrector(Identity[I], full, uncorrect)

    def choose[I, C](f: I => Corrector[I, C], uncorrect: C => I): Corrector[I, C] =
      Corrector[I, C](
        i => f(i).live(i),
        i => f(i).full(i),
        uncorrect)

    def choose[A](f: A => Corrector[A, A]): Corrector[A, A] =
      choose(f, Identity[A])
  }

  // ===================================================================================================================
  // Validation
  // ===================================================================================================================

  final case class Invalidator[+E, A](invalidate: A => Option[E]) extends AnyVal {
    @inline def apply(a: A): Option[E] =
      invalidate(a)

    def audit[AA <: A with AnyRef](a: AA): E \/ a.type =
      invalidate(a) match {
        case None => \/-[a.type](a)
        case Some(e) => -\/(e)
      }

    def contramap[B](f: B => A): Invalidator[E, B] =
      Invalidator(invalidate compose f)

    def mapError[F](f: E => F): Invalidator[F, A] =
      Invalidator(invalidate(_).map(f))

    def merge[EE >: E](that: Invalidator[EE, A])(implicit E: Semigroup[EE]): Invalidator[EE, A] =
      Invalidator(a =>
        (invalidate(a), that.invalidate(a)) match {
          case (None      , None      ) => None
          case (e@ Some(_), None      ) => e
          case (None      , e@ Some(_)) => e
          case (Some(e1)  , Some(e2)  ) => Some(E.append(e1, e2))
        }
      )

    def liftOption: Invalidator[E, Option[A]] =
      Invalidator(_ flatMap invalidate)

    def whenValid[EE >: E](next: Invalidator[EE, A]): Invalidator[EE, A] =
      Invalidator(a => invalidate(a) orElse next.invalidate(a))

    def whenValid[EE >: E](next: EndoValidator[EE, A]): EndoValidator[EE, A] =
      EndoValidator(next.corrector, whenValid(next.invalidator))

    def toEndoValidator: EndoValidator[E, A] =
      EndoValidator(EndoCorrector.id, Invalidator(invalidate))

    def toValidator: Validator[E, A, A, A] =
      toEndoValidator.toValidator

    def toAuditor: Auditor[E, A, A] =
      Auditor(a => invalidate(a) match {
        case None => \/-(a)
        case Some(e) => -\/(e)
      })
  }

  object Invalidator {
    def id[A]: Invalidator[Nothing, A] =
      Invalidator(_ => None)

    def choose[E, A](f: A => Invalidator[E, A]): Invalidator[E, A] =
      Invalidator(e => f(e).invalidate(e))

    implicit def VarianceBypass[E, A](a: Invalidator[E, A]): VarianceBypass[E, A] = new VarianceBypass(a.invalidate)
    final class VarianceBypass[E, A](private val invalidate: A => Option[E]) extends AnyVal {

      def liftTraverse[T[_]](implicit T: Traverse[T], E: Semigroup[E]): Invalidator[E, T[A]] =
        Invalidator(ta => {
          val ok: E \/ Unit = Auditor.unitResult
          val result: E \/ Unit = T.traverse_(ta)(invalidate(_).fold(ok)(-\/(_)))(AccumuateErrors.applicativeInstance)
          result.fold(Some(_), _ => None)
        })
    }
  }

  // ===================================================================================================================

  final case class Auditor[+E, C, V](audit: C => E \/ V) extends AnyVal {
    @inline def apply(c: C): E \/ V =
      audit(c)

    def validity(c: C): Validity =
      Valid when apply(c).isRight

    def contramap[A](f: A => C): Auditor[E, A, V] =
      Auditor(audit compose f)

    def andThen[EE >: E, A](next: Auditor[EE, V, A]): Auditor[EE, C, A] =
      Auditor(audit(_).flatMap(next.audit))

    def mapError[F](f: E => F): Auditor[F, C, V] =
      Auditor(audit(_) leftMap f)

    def mapValid[A](f: V => A): Auditor[E, C, A] =
      Auditor(audit(_) map f)

    def appendInvalidator[EE >: E](i: Invalidator[EE, V]): Auditor[EE, C, V] =
      Auditor(c => audit(c).flatMap(v => i.invalidate(v) match {
        case None => \/-(v)
        case Some(e) => -\/(e)
      }))

    def pair[EE >: E, CC, VV](implicit E: Semigroup[EE], C: GenTuple[C, C, CC], V: GenTuple[V, V, VV]): Auditor[EE, CC, VV] =
      this tuple (this: Auditor[EE, C, V])

    def tuple[EE >: E, C2, V2, CC, VV](b: Auditor[EE, C2, V2])(implicit E: Semigroup[EE], C: GenTuple[C, C2, CC], V: GenTuple[V, V2, VV]): Auditor[EE, CC, VV] =
      Auditor[EE, CC, VV](cc => {
        val (c1, c2) = C.init(cc)
        def v1 = audit(c1)
        def v2 = b.audit(c2)
        AccumuateErrors(v1, v2)(V.append)(E)
      })

    def liftOption: Auditor[E, Option[C], Option[V]] =
      Auditor({
        case None    => \/-(None)
        case Some(c) => audit(c).map(Some(_))
      })

    def toInvalidator: Invalidator[E, C] =
      Invalidator(audit(_).swap.toOption)

    def toValidator: Validator[E, C, C, V] =
      Validator(Corrector.id, this)

    def vector[EE >: E](implicit e: Semigroup[EE]): Auditor[EE, Vector[C], Vector[V]] =
      Auditor.traverse[EE, Vector, C, V](audit)
  }

  object Auditor {
    val unitResult = \/-(())

    def id[A]: Auditor[Nothing, A, A] =
      Auditor(\/-(_))

    def fail[A]: Auditor[A, A, A] =
      Auditor(-\/(_))

    def choose[E, C, V](f: C => Auditor[E, C, V]): Auditor[E, C, V] =
      Auditor(c => f(c).audit(c))

    def test[E, A](findErr: A => Option[E]): Auditor[E, A, A] =
      apply(a =>
        findErr(a) match {
          case None    => \/-(a)
          case Some(e) => -\/(e)
        }
      )

    def optionFn[E, A, B](f: A => Option[B])(invalidity: A => E): Auditor[E, A, B] =
      apply(a => f(a) match {
        case Some(b) => \/-(b)
        case None => -\/(invalidity(a))
      })

    def option[E, A](invalidity: => E): Auditor[E, Option[A], A] =
      optionFn[E, Option[A], A](Identity.apply)(_ => invalidity)

    def sequence[E, F[_], A](implicit T: Traverse[F], E: Semigroup[E]): Auditor[E, F[E \/ A], F[A]] =
      Auditor(i => T.sequence(i)(AccumuateErrors.applicativeInstance(E)))

    def traverse[E, F[_], A, B](f: A => E \/ B)(implicit T: Traverse[F], E: Semigroup[E]): Auditor[E, F[A], F[B]] =
      Auditor(fa => T.traverse(fa)(f)(AccumuateErrors.applicativeInstance(E)))

    implicit def VarianceBypass[E, C, V](a: Auditor[E, C, V]): VarianceBypass[E, C, V] = new VarianceBypass(a.audit)
    final class VarianceBypass[E, C, V](private val audit: C => E \/ V) extends AnyVal {

      def liftTraverse[T[_]](implicit T: Traverse[T], E: Semigroup[E]): Auditor[E, T[C], T[V]] =
        Auditor.traverse(audit)
    }
  }

  object AccumuateErrors {

    def apply[E, A, B, C](x: E \/ A, y: E \/ B)(f: (A, B) => C)(implicit E: Semigroup[E]): E \/ C =
      (x, y) match {
        case (\/-(a), \/-(b)) => \/-(f(a, b))
        case (\/-(_), e@ -\/(_)) => e
        case (e@ -\/(_), \/-(_)) => e
        case (-\/(e1), -\/(e2)) => -\/(E.append(e1, e2))
      }

    def applicativeInstance[E](implicit E: Semigroup[E]): Applicative[E \/ ?] =
      new Applicative[E \/ ?] {
        override def point[A](a: => A) = \/-(a)
        override def ap[A, B](fa: => E \/ A)(fab: => E \/ (A => B)) = AccumuateErrors(fa, fab)((a, f) => f(a))(E)
      }
  }

  // ===================================================================================================================
  // Correction + Validation
  // ===================================================================================================================

  final case class EndoValidator[+E, A](corrector: EndoCorrector[A],
                                        invalidator: Invalidator[E, A]) {

    def xmapInput[B](g: A => B)(f: B => A): EndoValidator[E, B] =
      EndoValidator(corrector.xmap(g)(f), invalidator.contramap(f))

    def imapInput[B](iso: B <=> A): EndoValidator[E, B] =
      xmapInput(iso.from)(iso.to)

    def append[EE >: E](that: EndoValidator[EE, A])(implicit E: Semigroup[EE]): EndoValidator[EE, A] =
      EndoValidator(
        corrector append that.corrector,
        invalidator merge that.invalidator)

    def prepend[EE >: E](that: EndoValidator[EE, A])(implicit E: Semigroup[EE]): EndoValidator[EE, A] =
      EndoValidator(
        corrector prepend that.corrector,
        invalidator merge that.invalidator)

    def appendCorrector(c: EndoCorrector[A]): EndoValidator[E, A] =
      EndoValidator(corrector append c, invalidator)

    def prependCorrector(c: EndoCorrector[A]): EndoValidator[E, A] =
      EndoValidator(corrector prepend c, invalidator)

    def mapInvalidator[F](f: Invalidator[E, A] => Invalidator[F, A]): EndoValidator[F, A] =
      EndoValidator(corrector, f(invalidator))

    def addInvalidator[EE >: E](that: Invalidator[EE, A])(implicit E: Semigroup[EE]): EndoValidator[EE, A] =
      mapInvalidator(_ merge that)

    def toValidator: Validator[E, A, A, A] =
      Validator(corrector.toCorrector, invalidator.toAuditor)
  }

  object EndoValidator {
    def id[A]: EndoValidator[Nothing, A] =
      EndoValidator(EndoCorrector.id, Invalidator.id)

    def choose[E, A](f: A => EndoValidator[E, A]): EndoValidator[E, A] =
      EndoValidator(EndoCorrector.choose(f.andThen(_.corrector)), Invalidator.choose(f.andThen(_.invalidator)))
  }

  // ===================================================================================================================

  final case class Validator[+E, I, C, V](corrector: Corrector[I, C], auditor: Auditor[E, C, V]) {
    def apply(i: I): E \/ V =
      auditor(corrector(i))

    def validity(i: I): Validity =
      Valid when apply(i).isRight

    def mapCorrector[A](f: Corrector[I, C] => Corrector[A, C]): Validator[E, A, C, V] =
      Validator(f(corrector), auditor)

    def mapAuditor[EE, VV](f: Auditor[E, C, V] => Auditor[EE, C, VV]): Validator[EE, I, C, VV] =
      Validator(corrector, f(auditor))

    def mapError[F](f: E => F): Validator[F, I, C, V] =
      mapAuditor(_.mapError(f))

    def mapValid[A](f: V => A): Validator[E, I, C, A] =
      mapAuditor(_.mapValid(f))

    def xmapInput[B](g: I => B)(f: B => I): Validator[E, B, C, V] =
      mapCorrector(_.xmapInput(g)(f))

    def imapInputZ[B](iso: B <=> I): Validator[E, B, C, V] =
      xmapInput(iso.from)(iso.to)

    def imapInput[B](iso: Iso[B, I]): Validator[E, B, C, V] =
      xmapInput(iso.reverseGet)(iso.get)

    def xmapCorrected[B](g: C => B)(f: B => C): Validator[E, I, B, V] =
      Validator(corrector.xmapCorrected(g)(f), auditor.contramap(f))

    def imapCorrectedZ[B](iso: B <=> C): Validator[E, I, B, V] =
      xmapCorrected(iso.from)(iso.to)

    def imapCorrected[B](iso: Iso[B, C]): Validator[E, I, B, V] =
      xmapCorrected(iso.reverseGet)(iso.get)

    def appendInvalidator[EE >: E](i: Invalidator[EE, V]): Validator[EE, I, C, V] =
      mapAuditor(_.appendInvalidator(i))

    def andThenAuditor[EE >: E, A](a: Auditor[EE, V, A]): Validator[EE, I, C, A] =
      mapAuditor(_.andThen(a))

    def andThen[EE >: E, C2, V2](next: Validator[EE, V, C2, V2]): Validator[EE, I, C, V2] =
      mapAuditor(_.andThen(next.toAuditor[EE]))

    def toAuditor[EE >: E]: Auditor[EE, I, V] =
      Auditor(apply)

    def toInvalidator[EE >: E]: Invalidator[EE, I] =
      toAuditor.toInvalidator

    def pair[EE >: E, II, CC, VV](implicit E: Semigroup[EE], I: GenTuple[I, I, II], C: GenTuple[C, C, CC], V: GenTuple[V, V, VV]): Validator[EE, II, CC, VV] =
      this tuple (this: Validator[EE, I, C, V])

    def tuple[EE >: E, I2, C2, V2, II, CC, VV](that: Validator[EE, I2, C2, V2])(implicit E: Semigroup[EE], I: GenTuple[I, I2, II], C: GenTuple[C, C2, CC], V: GenTuple[V, V2, VV]): Validator[EE, II, CC, VV] =
      Validator(
        corrector tuple that.corrector,
        auditor tuple that.auditor)

//    def product[EE >: E, I2, C2, V2](that: Validator[EE, I2, C2, V2])(implicit E: Semigroup[EE]): Validator[EE, (I, I2), (C, C2), (V, V2)] =
//      this tuple that

    def vectorWithGaps[G, EE >: E](implicit e: Semigroup[EE]): Validator[EE, Vector[G \/ I], Vector[G \/ C], Vector[V]] =
      Validator[EE, Vector[G \/ I], Vector[G \/ C], Vector[V]](
        corrector.vectorWithGaps[G],
        auditor.vector(e).contramap(_.flatMap(_.toList)))
  }

  object Validator {
    def id[A]: Validator[Nothing, A, A, A] =
      Validator(Corrector.id, Auditor.id)

    def fail[A]: Validator[A, A, A, A] =
      Validator(Corrector.id, Auditor.fail)

    def option[A, E](invalidity: => E): Validator[E, Option[A], Option[A], A] =
      Validator(Corrector.id, Auditor.option(invalidity))

    def choose[E, I, V](f: I => Validator[E, I, I, V]): Validator[E, I, I, V] =
      Validator(Corrector.choose(f.andThen(_.corrector)), Auditor.choose(f.andThen(_.auditor)))

    def choose[E, I, C, V](f: I => Validator[E, I, C, V], uncorrect: C => I): Validator[E, I, C, V] =
      Validator(Corrector.choose(f.andThen(_.corrector), uncorrect), Auditor.choose(c => f(uncorrect(c)).auditor))
  }
}
