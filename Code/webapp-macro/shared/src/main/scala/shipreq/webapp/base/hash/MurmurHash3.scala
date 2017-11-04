package shipreq.webapp.base.hash

import scala.util.hashing.{MurmurHash3 => *}
import *.{mix, mixLast, finalizeHash}

/**
 * MurmurHash 3.
 *
 * https://en.wikipedia.org/wiki/MurmurHash
 */
object MurmurHash3 extends Hash.Algorithm {
  private val joinSeed  = *.stringHash("join")
  private val unordSeed = *.stringHash("unord")

  override def joinHashes(hashes: List[Int]): Int = {
    var n = 0
    var h = joinSeed
    var elems = hashes
    while (!elems.isEmpty) {
      val head = elems.head
      val tail = elems.tail
      h = mix(h, head) // look ma, no hashing!
      n += 1
      elems = tail
    }
    finalizeHash(h, n)
  }

  override implicit val hashBoolean: HashFn[Boolean] =
    HashFn(a => if (a) 1231 else 1237)

  override implicit val hashString: HashFn[String] =
    HashFn(*.stringHash)

  override implicit def hashMap[K: HashFn, V: HashFn]: HashFn[Map[K, V]] =
    HashFn(unorderedHash(_, *.mapSeed))

  override implicit def hashSet[A: HashFn]: HashFn[Set[A]] =
    HashFn(unorderedHash(_, *.setSeed))

  override implicit def hashList[A: HashFn]: HashFn[List[A]] =
    HashFn(listHash(_, *.seqSeed))

  override implicit def hashVector[A: HashFn]: HashFn[Vector[A]] =
    HashFn(orderedHash(_, *.seqSeed))

  override protected def _hashPair[
      @specialized(Int, Long, Char, Boolean) A: HashFn,
      @specialized(Int, Long, Char, Boolean) B: HashFn]: HashFn[(A, B)] =
    HashFn(t => mixLast(Hash(t._1), Hash(t._2)))

  override def hashUnordered[T[x] <: TraversableOnce[x], A: HashFn]: HashFn[T[A]] =
    HashFn(unorderedHash(_, unordSeed))

  override implicit val hashChar: HashFn[Char] = HashFn { a =>
    val h = mix(0, a.toInt)
    finalizeHash(h, 2)
  }

  // ===================================================================================================================
  // Copied from Clojure
  // Changed to remove special case for 0.
  // https://github.com/clojure/clojure/blob/6aaaa0a88da15fb814e12a8a4e9af864edfafd6f/src/jvm/clojure/lang/Murmur3.java

  override implicit val hashInt: HashFn[Int] = HashFn { a =>
    val h = mix(0, a)
    finalizeHash(h, 4)
  }

  override implicit val hashLong: HashFn[Long] = HashFn { a =>
    val low  = a.toInt
    val high = (a >>> 32).toInt
    var h = mix(0, low)
        h = mix(h, high)
    finalizeHash(h, 8)
  }

  // ===================================================================================================================
  // Copied from Scala 2.11.7
  // Changed to use .hash instead of .##
  // https://github.com/scala/scala/blob/v2.11.7/src/library/scala/util/hashing/MurmurHash3.scala

  /** Compute a hash that is symmetric in its arguments - that is a hash
    *  where the order of appearance of elements does not matter.
    *  This is useful for hashing sets, for example.
    */
  private final def unorderedHash[A: HashFn](xs: TraversableOnce[A], seed: Int): Int = {
    var a, b, n = 0
    var c = 1
    xs foreach { x =>
      val h = Hash(x)
      a += h
      b ^= h
      if (h != 0) c *= h
      n += 1
    }
    var h = seed
    h = mix(h, a)
    h = mix(h, b)
    h = mixLast(h, c)
    finalizeHash(h, n)
  }

  /** Compute a hash that depends on the order of its arguments.
    */
  private final def orderedHash[A: HashFn](xs: TraversableOnce[A], seed: Int): Int = {
    var n = 0
    var h = seed
    xs foreach { x =>
      h = mix(h, Hash(x))
      n += 1
    }
    finalizeHash(h, n)
  }

  private final def listHash[A: HashFn](xs: scala.collection.immutable.List[A], seed: Int): Int = {
    var n = 0
    var h = seed
    var elems = xs
    while (!elems.isEmpty) {
      val head = elems.head
      val tail = elems.tail
      h = mix(h, Hash(head))
      n += 1
      elems = tail
    }
    finalizeHash(h, n)
  }
}