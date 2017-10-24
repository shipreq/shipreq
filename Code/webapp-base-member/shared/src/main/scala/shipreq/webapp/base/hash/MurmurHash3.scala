package shipreq.webapp.base.hash

import scala.util.hashing.{MurmurHash3 => *}
import Hash.HashableValueOps
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

  implicit override val hashBoolean                  : Hash[Boolean]   = Hash.fn(a => if (a) 1231 else 1237)
  implicit override val hashString                   : Hash[String]    = Hash.fn(*.stringHash)
  override implicit def hashPair   [A: Hash, B: Hash]: Hash[(A, B)]    = Hash.fn(t => mixLast(t._1.hash, t._2.hash))
  implicit override def hashMap    [K: Hash, V: Hash]: Hash[Map[K, V]] = Hash.fn(unorderedHash(_, *.mapSeed))
  implicit override def hashSet    [A: Hash]         : Hash[Set[A]]    = Hash.fn(unorderedHash(_, *.setSeed))
  implicit override def hashList   [A: Hash]         : Hash[List[A]]   = Hash.fn(listHash(_, *.seqSeed))
  implicit override def hashVector [A: Hash]         : Hash[Vector[A]] = Hash.fn(orderedHash(_, *.seqSeed))

  override def hashUnordered[T[x] <: TraversableOnce[x], A: Hash]: Hash[T[A]] =
    Hash.fn(unorderedHash(_, unordSeed))

  implicit override val hashChar: Hash[Char] = Hash.fn { a =>
    val h = mix(0, a.toInt)
    finalizeHash(h, 2)
  }

  // ===================================================================================================================
  // Copied from Clojure
  // Changed to remove special case for 0.
  // https://github.com/clojure/clojure/blob/6aaaa0a88da15fb814e12a8a4e9af864edfafd6f/src/jvm/clojure/lang/Murmur3.java

  implicit override val hashInt: Hash[Int] = Hash.fn { a =>
    val h = mix(0, a)
    finalizeHash(h, 4)
  }

  implicit override val hashLong: Hash[Long] = Hash.fn { a =>
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
  private final def unorderedHash[A: Hash](xs: TraversableOnce[A], seed: Int): Int = {
    var a, b, n = 0
    var c = 1
    xs foreach { x =>
      val h = x.hash
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
  private final def orderedHash[A: Hash](xs: TraversableOnce[A], seed: Int): Int = {
    var n = 0
    var h = seed
    xs foreach { x =>
      h = mix(h, x.hash)
      n += 1
    }
    finalizeHash(h, n)
  }

  private final def listHash[A: Hash](xs: scala.collection.immutable.List[A], seed: Int): Int = {
    var n = 0
    var h = seed
    var elems = xs
    while (!elems.isEmpty) {
      val head = elems.head
      val tail = elems.tail
      h = mix(h, head.hash)
      n += 1
      elems = tail
    }
    finalizeHash(h, n)
  }
}