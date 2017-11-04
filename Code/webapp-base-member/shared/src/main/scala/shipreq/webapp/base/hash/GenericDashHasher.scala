package shipreq.webapp.base.hash

import japgolly.microlibs.nonempty._
import japgolly.microlibs.recursion._
import nyaya.util.Multimap
import scalaz.{Functor, \/}
import shipreq.base.util.TaggedTypes.TaggedType
import shipreq.base.util._

final class GenericDashHasher(algorithm: Hash.Algorithm) {
  import algorithm._

  /**
   * Mixes the hash with a hash of `name` so that identical values in different places don't have identical hashes.
   *
   * @param name Some arbitrary string.
   */
  def withName[A](name: String, h: HashFn[A]): HashFn[A] = {
    val q = hashString(name) :: Nil
    HashFn[A](a => joinHashes(h(a) :: q))
  }

  def hashTaggedType[T <: TaggedType](implicit h: HashFn[T#U]): HashFn[T] =
    h.contramap(_.value)

  implicit def hashMultimap[K, L[_], V](implicit h: HashFn[Map[K, L[V]]]): HashFn[Multimap[K, L, V]] =
    h.contramap(_.m)

  private val hashNone = Hash("∅")

  implicit def hashOption[A](implicit h: HashFn[A]): HashFn[Option[A]] = {
    val hashSome = withName("!", h)
    // Avoid boxing and closure creation by using .isEmpty & .get
    HashFn(o => if (o.isEmpty) hashNone else hashSome(o.get))
  }

  implicit def hashNEV[A: HashFn]: HashFn[NonEmptyVector[A]] =
    HashFn.by(_.whole)

  implicit def hashNES[A: HashFn]: HashFn[NonEmptySet[A]] =
    HashFn.by(_.whole)

  implicit def hashTrie[K: HashFn, V: HashFn]: HashFn[MTrie.Trie[K, V]] = {
    import MTrie.{Branch, Node, Trie, Value}
    implicit      val value : HashFn[Value [K, V]] = hashCaseClass
    implicit      val valueO                       = hashOption(value)
    implicit lazy val branch: HashFn[Branch[K, V]] = hashCaseClass
    implicit lazy val node  : HashFn[Node  [K, V]] = HashFn(_.fold(branch.hashFn, value.hashFn))
    implicit lazy val trie  : HashFn[Trie  [K, V]] = hashMap
    trie
  }

  implicit def hashVectorTree[A](implicit ha: HashFn[A]): HashFn[VectorTree[A]] = {
    import VectorTree._

    implicit lazy val node: HashFn[Node[A]] =
      HashFn[Node[A]](n => joinHashes(
        ha(n.value) :: children(n.children) :: Nil))

    implicit lazy val children: HashFn[Children[A]] =
      hashVector[Node[A]]

    hashCaseClass
  }

  implicit def hashIMap[K, V: HashFn]: HashFn[IMap[K, V]] =
    hashUnordered[Iterable, V].contramap(_.values)

  implicit def disjunction[A, B](implicit ha: HashFn[A], r: HashFn[B]): HashFn[A \/ B] = {
    val l = withName("!", ha)
    HashFn(_.fold(l.hashFn, r.hashFn))
  }

  def hashISubset[A: HashFn]: HashFn[ISubset[A]] = {
    import ISubset._
    implicit val anes = hashNES[A]
    implicit val all : HashFn[All [A]] = hashConstClass("Al")
    implicit val only: HashFn[Only[A]] = withName("On", hashCaseClass)
    implicit val not : HashFn[Not [A]] = withName("No", hashCaseClass)
    hashADT
  }

  def hashFix[F[_]: Functor](algebra: Algebra[F, Int]): HashFn[Fix[F]] =
    HashFn(Recursion.cata(algebra))
}
