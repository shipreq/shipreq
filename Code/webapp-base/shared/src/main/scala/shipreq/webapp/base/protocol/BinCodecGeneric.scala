package shipreq.webapp.base.protocol

import boopickle._
import japgolly.microlibs.nonempty._
import japgolly.microlibs.recursion._
import monocle.Iso
import nyaya.util.{MultiValues, Multimap}
import scalaz.{-\/, Functor, \&/, \/, \/-}
import scalaz.Isomorphism.<=>
import shipreq.base.util._
import shipreq.base.util.univeq._
import BoopickleMacros._

object BinCodecGeneric extends BasicImplicitPicklers with TuplePicklers {

  @inline implicit class PicklerExt[A](private val p: Pickler[A]) extends AnyVal {
    def imap[B](iso: Iso[A, B]): Pickler[B] =
      p.xmap(iso.get)(iso.reverseGet)

    /** Unpickling is safe but pickling will break if you pass it b⊄A */
    @inline def unsafeWiden[B >: A]: Pickler[B] =
//      new Pickler[B] {
//        override def pickle(b: B)(implicit state: PickleState): Unit = p.pickle(b.asInstanceOf[A])
//        override def unpickle(implicit state: UnpickleState)  : A    = p.unpickle
//      }
      p.asInstanceOf[Pickler[B]]
  }

  def pickleLazily[A](f: => Pickler[A]): Pickler[A] = {
    lazy val p = f
    new Pickler[A] {
      override def pickle(a: A)(implicit state: PickleState): Unit = p.pickle(a)
      override def unpickle(implicit state: UnpickleState)  : A    = p.unpickle
    }
  }

  def pickleEnum[V: UnivEq](nev: NonEmptyVector[V]): Pickler[V] =
    new Pickler[V] {
      val vs = nev.whole
      val vtoi = vs.zipWithIndex.toMap
      assert(vtoi.size == nev.length, s"Duplicates found in $nev")
      override def pickle(v: V)(implicit state: PickleState): Unit = {
        val i = vtoi(v)
        state.enc.writeInt(i)
      }
      override def unpickle(implicit state: UnpickleState): V =
        state.dec.readIntCode match {
          case Right(i) => vs(i)
          case Left(_)  => throw new IllegalArgumentException("Unknown coding")
        }
    }

  def pickleTaggedL[T <: TaggedTypes.TaggedLong]  (apply: Long   => T) = xmap(apply)(_.value)
  def pickleTaggedI[T <: TaggedTypes.TaggedInt]   (apply: Int    => T) = xmap(apply)(_.value)
  def pickleTaggedS[T <: TaggedTypes.TaggedString](apply: String => T) = xmap(apply)(_.value)

  def pickleBool[T](iso: Boolean <=> T): Pickler[T] =
    xmap(iso.to)(iso.from)

  def pickleIsoBoolValues[B <: IsoBool[B], A: Pickler]: Pickler[IsoBool.Values[B, A]] =
    xmap[IsoBool.Values[B, A], (A, A)](x => IsoBool.Values(pos = x._1, neg = x._2))(x => (x.pos, x.neg))

  implicit def pickleMap[K: Pickler, V: Pickler]: Pickler[Map[K, V]] =
    mapPickler[K, V, Map]

  def pickleIMap[K: UnivEq, V: Pickler](empty: IMap[K, V]): Pickler[IMap[K, V]] =
    xmap(empty ++ (_: Iterable[V]))(_.values)

  def pickleNonEmpty[N, E](f: N => E)(implicit p: Pickler[E], proof: NonEmpty.Proof[E, N]): Pickler[N] =
    p.xmap(NonEmpty require_! _)(f)

  implicit def pickleNonEmptyMono[A](implicit p: Pickler[A], proof: NonEmpty.ProofMono[A]): Pickler[NonEmpty[A]] =
    pickleNonEmpty(_.value)

  implicit def pickleNEV[A](implicit p: Pickler[Vector[A]]): Pickler[NonEmptyVector[A]] =
    pickleNonEmpty(_.whole)

  implicit def pickleNES[A: UnivEq](implicit p: Pickler[Set[A]]): Pickler[NonEmptySet[A]] =
    pickleNonEmpty(_.whole)

  implicit def pickleISubset[A: UnivEq](implicit as: Pickler[NonEmptySet[A]]): Pickler[ISubset[A]] = {
    import ISubset._
    implicit val a: Pickler[All [A]] = pickleCaseClass
    implicit val o: Pickler[Only[A]] = pickleCaseClass
    implicit val n: Pickler[Not [A]] = pickleCaseClass
    pickleADT
  }

  implicit def pickleMultimap[K: UnivEq, L[_], V](implicit p: Pickler[Map[K, L[V]]], l: MultiValues[L]): Pickler[Multimap[K, L, V]] =
    p.xmap(Multimap(_))(_.m) // TODO optimise

  implicit def pickleSetDiff[A: UnivEq](implicit rw: Pickler[Set[A]]): Pickler[SetDiff[A]] =
    pickleCaseClass

  implicit def pickleTrie[K: Pickler, V: Pickler]: Pickler[MTrie.Trie[K, V]] = {
    import MTrie.{Branch, Node, Trie, Value}
    implicit      val value : Pickler[Value [K, V]] = pickleCaseClass
    implicit      val valueO                        = optionPickler(value)
    implicit lazy val branch: Pickler[Branch[K, V]] = pickleCaseClass
    implicit lazy val node  : Pickler[Node  [K, V]] = pickleADT
    implicit lazy val trie  : Pickler[Trie  [K, V]] = pickleLazily(pickleMap)
    trie
  }

  implicit def pickleVectorTree[A: Pickler]: Pickler[VectorTree[A]] = {
    import VectorTree._
    object N extends Pickler[Node[A]] {
      val ch = iterablePickler[Node[A], Vector](this, implicitly)
      override def pickle(node: Node[A])(implicit state: PickleState): Unit = {
        state.pickle(node.value)
        state.pickle(node.children)(ch)
      }
      override def unpickle(implicit state: UnpickleState): Node[A] =
        Node.apply(state.unpickle[A], state.unpickle(ch))
    }
    N.ch.xmap(VectorTree.apply)(_.children)
  }

  implicit def pickleXor[A: Pickler, B: Pickler]: Pickler[A \/ B] = {
    implicit val l = pickleCaseClass[-\/[A]]
    implicit val r = pickleCaseClass[\/-[B]]
    pickleADT
  }

  implicit def pickleIor[A: Pickler, B: Pickler]: Pickler[A \&/ B] = {
    import \&/._
    val ths = pickleCaseClass[This[A]]
    val tht = pickleCaseClass[That[B]]
    val bth = pickleCaseClass[Both[A, B]]
    unsafeSelector(ths, tht, bth) {
      case _: This[A]    => 0
      case _: That[B]    => 1
      case _: Both[A, B] => 2
    }
  }

  def pickleFix[F[_]: Functor](implicit p: Pickler[F[Unit]]): Pickler[Fix[F]] =
    new Pickler[Fix[F]] {
      override def pickle(f: Fix[F])(implicit state: PickleState): Unit = {

        // val fUnit = Functor[F].void(f.unfix)
        // p.pickle(fUnit)
        // Functor[F].map(f.unfix)(pickle)

        // Compared to ↑, this ↓ is generally on-par for small trees, and around 30% faster for larger, deeper trees

        val fields = new collection.mutable.ArrayBuffer[Fix[F]](32)
        val fUnit = Functor[F].map(f.unfix) { a =>
          fields += a
          ()
        }
        p.pickle(fUnit)
        fields.foreach(pickle)

        ()
      }

      override def unpickle(implicit state: UnpickleState) = {
        val fUnit = p.unpickle
        Fix(Functor[F].map(fUnit)(_ => unpickle))
      }
    }
}
