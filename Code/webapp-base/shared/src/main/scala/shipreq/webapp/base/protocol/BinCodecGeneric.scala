package shipreq.webapp.base.protocol

import boopickle._
import japgolly.nyaya.util.{Multimap, MultiValues}
import scalaz.{\/, -\/, \/-, \&/}
import shipreq.base.util._
import BoopickleMacros._

object BinCodecGeneric extends BasicImplicitPicklers with TuplePicklers {
  import shipreq.webapp.base.data.DataIdAux

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

  def pickleBool[T](iso: IsoBool[T]): Pickler[T] =
    xmap(iso.to)(iso.from)

  implicit def pickleMap[K: Pickler, V: Pickler]: Pickler[Map[K, V]] =
    mapPickler[K, V, Map]

  def pickleIMap[K: UnivEq, V: Pickler](empty: IMap[K, V]): Pickler[IMap[K, V]] =
    xmap(empty ++ (_: Iterable[V]))(_.values)

  @inline def pickleIMapD[K: UnivEq : Pickler, V: Pickler](implicit d: DataIdAux[V, K]): Pickler[IMap[K, V]] =
    pickleIMap(d.emptyIMap)

  implicit def pickleNEV[A](implicit p: Pickler[Vector[A]]): Pickler[NonEmptyVector[A]] =
    p.xmap(l => NonEmptyVector(l.head, l.tail))(_.whole)

  implicit def pickleNES[A: UnivEq](implicit p: Pickler[Set[A]]): Pickler[NonEmptySet[A]] =
    p.xmap(l => NonEmptySet(l.head, l.tail))(_.whole)

  implicit def pickleNonEmpty[A](implicit a: Pickler[A], proof: NonEmpty.ProofA[A]): Pickler[NonEmpty[A]] =
    a.xmap(NonEmpty.tryO(_).getOrElse(sys error "Invalid data"))(_.value)

  implicit def pickleISubset[A: UnivEq](implicit as: Pickler[NonEmptySet[A]]): Pickler[ISubset[A]] = {
    import ISubset._
    implicit val a: Pickler[All [A]] = pickleCaseClass
    implicit val o: Pickler[Only[A]] = pickleCaseClass
    implicit val n: Pickler[Not [A]] = pickleCaseClass
    pickleADT
  }

  implicit def pickleMultimap[K: UnivEq, L[_], V](implicit p: Pickler[Map[K, L[V]]], l: MultiValues[L]): Pickler[Multimap[K, L, V]] =
    p.xmap(Multimap(_))(_.m)

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
}
