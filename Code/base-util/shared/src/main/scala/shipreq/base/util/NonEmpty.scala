package shipreq.base.util

import scalaz.{\/, -\/, \/-}

/**
 * Type indicating that its value has been proven to be non-empty.
 */
case class NonEmpty[A] private[NonEmpty] (value: A) extends AnyVal

object NonEmpty {

  @inline implicit def autoNonEmptyValue[A](n: NonEmpty[A]): A = n.value

  @inline implicit def nonEmptyUnivEq[A: UnivEq]: UnivEq[NonEmpty[A]] = UnivEq.force

  @inline def force[A](a: A): NonEmpty[A] = NonEmpty(a)

  def tryO[I, O](i: I)(implicit proof: Proof[I, O]): Option[NonEmpty[O]] =
    proof.test(i).map(NonEmpty(_))

  def tryD[I, O](i: I)(implicit proof: Proof[I, O]): I \/ NonEmpty[O] =
    proof.test(i).fold[I \/ NonEmpty[O]](-\/(i))(o => \/-(NonEmpty(o)))

  // -------------------------------------------------------------------------------------------------------------------
  //  Proofs

  case class Proof[I, O](test: I => Option[O]) extends AnyVal

  type ProofA[A] = Proof[A, A]

  object Proof {
    def testEmptiness[A](f: A => Boolean): Proof[A, A] =
      Proof(a => if (f(a)) None else Some(a))

    def testNonEmptiness[A](f: A => Boolean): Proof[A, A] =
      Proof(a => if (f(a)) Some(a) else None)
  }

  implicit def proveNES[A: UnivEq]: Proof[Set[A], NonEmptySet[A]] =
    Proof(NonEmptySet.option[A])

  implicit def proveNEV[A]: Proof[Vector[A], NonEmptyVector[A]] =
    Proof(NonEmptyVector.option[A])

  implicit def proveSetDiff[A]: ProofA[SetDiff[A]] =
    Proof.testEmptiness(_.isEmpty)

//  implicit def proveIMap[M <: IMapBase[K, V, T], K, V, T <: IMapBase[K, V, T]]: ProofA[M] =
//    Proof.testEmptiness(_.isEmpty)

  implicit def proveIMap[K, V]: ProofA[IMap[K, V]] =
    Proof.testEmptiness(_.isEmpty)

  implicit def proveMap[M <: Map[K, V], K, V]: ProofA[M] =
    Proof.testEmptiness(_.isEmpty)
}