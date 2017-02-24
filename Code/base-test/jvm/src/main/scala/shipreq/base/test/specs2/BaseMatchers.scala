package shipreq.base.test.specs2

import org.specs2.matcher.StandardMatchResults._
import org.specs2.matcher.Matcher
import org.specs2.matcher.Matchers._
import org.specs2.matcher.describe.{Diffable, PrimitiveDifference, PrimitiveIdentical}
import scala.reflect.ClassTag
import scalaz.{-\/, \/-}
import shipreq.base.test.MockOpTransformerResults
import shipreq.base.util.ErrorOr

object BaseMatchers {

  def match2[A, B](a: Matcher[A], b: Matcher[B]): Matcher[(A, B)] =
    (a, b).zip(i => i, i => i)

  def beAnError: Matcher[ErrorOr[_]] =
    beLike{ case -\/(_) => ok }

  def notBeAnError: Matcher[ErrorOr[_]] =
    beLike{ case \/-(_) => ok }

  def beNonErrorAnd[T](m: Matcher[T]): Matcher[ErrorOr[T]] =
    notBeAnError and (m ^^ { (e: ErrorOr[T]) => e.toOption.get })

  def beNonErrorOf[T](t: T): Matcher[ErrorOr[T]] =
    beNonErrorAnd(be_===(t))

  // -------------------------------------------------------------------------------------------------------------------

  class HaveRunOps[Op[_]] {
    import MockOpTransformerResults.isSubtype
    @inline private def M[A](implicit m: ClassTag[A]) = m
    type T = ClassTag[_ <: Op[_]]
    type LM = List[ClassTag[_ <: Op[_]]]
    type MR = MockOpTransformerResults[Op]

    def ops(expOps: ClassTag[_ <: Op[_]]*): Matcher[MockOpTransformerResults[Op]] = {
      val cmp = new Diffable[LM] {
        override def diff(a: LM, expected: LM) =
          if (a.length == expected.length && a.zip(expected).forall { case (x, y) => isSubtype(x, y) })
            PrimitiveIdentical(a) else PrimitiveDifference(a, expected)
      }
      beTypedEqualTo[LM](expOps.toList)(cmp) ^^ {(x: MR) => x.allOpTypes.toList}
    }

    def none = ops()

    def op[A <: Op[_]: ClassTag] =
      ops(M[A])

    def ops2[A <: Op[_]: ClassTag, B <: Op[_]: ClassTag] =
      ops(M[A], M[B])

    def ops3[A <: Op[_]: ClassTag, B <: Op[_]: ClassTag, C <: Op[_]: ClassTag] =
      ops(M[A], M[B], M[C])

    def ops4[A <: Op[_]: ClassTag, B <: Op[_]: ClassTag, C <: Op[_]: ClassTag, D <: Op[_]: ClassTag] =
      ops(M[A], M[B], M[C], M[D])

    def ops5[A <: Op[_]: ClassTag, B <: Op[_]: ClassTag, C <: Op[_]: ClassTag, D <: Op[_]: ClassTag, E <: Op[_]: ClassTag] =
      ops(M[A], M[B], M[C], M[D], M[E])

    def ops6[A <: Op[_]: ClassTag, B <: Op[_]: ClassTag, C <: Op[_]: ClassTag, D <: Op[_]: ClassTag, E <: Op[_]: ClassTag, F <: Op[_]: ClassTag] =
      ops(M[A], M[B], M[C], M[D], M[E], M[F])

    def anyBut(notExp: ClassTag[_ <: Op[_]]*): Matcher[MockOpTransformerResults[Op]] = {
      def matchingElem: Matcher[T] = ((x: T) => notExp.exists(y => isSubtype(x, y)))
      not(contain(matchingElem)) ^^ {(x: MR) => x.allOpTypes.toList}
    }

    def anyBut1[A <: Op[_]: ClassTag] =
      anyBut(M[A])
  }

  def haveRun[Op[_]] = new HaveRunOps[Op]

  type HaveRunF[A[_]] = HaveRunOps[A] => Matcher[MockOpTransformerResults[A]]

  def haveRun2[A[_], B[_]](a: HaveRunF[A], b: HaveRunF[B]): Matcher[(MockOpTransformerResults[A], MockOpTransformerResults[B])] =
    match2(a(new HaveRunOps[A]), b(new HaveRunOps[B]))
}
