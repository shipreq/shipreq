package shipreq.webapp.base.hash

/**
 * Hash just using values' internal hash codes.
 *
 * Useless really but here for playing around.
 */
object InternalHash extends Hash.Algorithm {

  override implicit val hashBoolean                      : HashFn[Boolean]   = HashFn.internal
  override implicit val hashChar                         : HashFn[Char]      = HashFn.internal
  override implicit val hashInt                          : HashFn[Int]       = HashFn.internal
  override implicit val hashLong                         : HashFn[Long]      = HashFn.internal
  override implicit val hashString                       : HashFn[String]    = HashFn.internal
  override implicit def hashMap    [K: HashFn, V: HashFn]: HashFn[Map[K, V]] = HashFn.internal
  override implicit def hashSet    [A: HashFn]           : HashFn[Set[A]]    = HashFn.internal
  override implicit def hashList   [A: HashFn]           : HashFn[List[A]]   = HashFn.internal
  override implicit def hashVector [A: HashFn]           : HashFn[Vector[A]] = HashFn.internal

  override protected def _hashPair[
      @specialized(Int, Long, Char, Boolean) A: HashFn,
      @specialized(Int, Long, Char, Boolean) B: HashFn]: HashFn[(A, B)] =
    HashFn.internal

  override def hashUnordered[T[x] <: TraversableOnce[x], A: HashFn]: HashFn[T[A]] = HashFn.internal

  override def joinHashes(hashes: List[Int]) = hashes.##
}