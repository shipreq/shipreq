package shipreq.webapp.base.hash

/**
 * Hash just using values' internal hash codes.
 *
 * Useless really but here for playing around.
 */
object InternalHash extends Hash.Algorithm {

  implicit override val hashBoolean                  : Hash[Boolean]   = Hash.internal
  implicit override val hashChar                     : Hash[Char]      = Hash.internal
  implicit override val hashInt                      : Hash[Int]       = Hash.internal
  implicit override val hashLong                     : Hash[Long]      = Hash.internal
  implicit override val hashString                   : Hash[String]    = Hash.internal
  implicit override def hashPair   [A: Hash, B: Hash]: Hash[(A, B)]    = Hash.internal
  implicit override def hashMap    [K: Hash, V: Hash]: Hash[Map[K, V]] = Hash.internal
  implicit override def hashSet    [A: Hash]         : Hash[Set[A]]    = Hash.internal
  implicit override def hashList   [A: Hash]         : Hash[List[A]]   = Hash.internal
  implicit override def hashVector [A: Hash]         : Hash[Vector[A]] = Hash.internal

  override def joinHashes(hashes: List[Int]) = hashes.##
  override def hashUnordered[T[x] <: TraversableOnce[x], A: Hash]: Hash[T[A]] = Hash.internal
}