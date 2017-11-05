package shipreq.webapp.base.hash

object Hash {

  @inline def apply[@specialized(Int, Long, Char, Boolean) A](a: A)(implicit h: HashFn[A]): Int =
    h.hashFn(a)


  trait Algorithm extends HashMacros {
    implicit val hashBoolean                      : HashFn[Boolean]
    implicit val hashChar                         : HashFn[Char]
    implicit val hashInt                          : HashFn[Int]
    implicit val hashLong                         : HashFn[Long]
    implicit val hashString                       : HashFn[String]
    implicit def hashMap    [K: HashFn, V: HashFn]: HashFn[Map[K, V]]
    implicit def hashSet    [A: HashFn]           : HashFn[Set[A]]
    implicit def hashList   [A: HashFn]           : HashFn[List[A]]
    implicit def hashVector [A: HashFn]           : HashFn[Vector[A]]

    protected def _hashPair[
      @specialized(Int, Long, Char, Boolean) A: HashFn,
      @specialized(Int, Long, Char, Boolean) B: HashFn]: HashFn[(A, B)]

    @inline final implicit def hashPair[
      @specialized(Int, Long, Char, Boolean) A: HashFn,
      @specialized(Int, Long, Char, Boolean) B: HashFn]: HashFn[(A, B)] =
      _hashPair[A, B]

    def hashUnordered[T[x] <: TraversableOnce[x], A: HashFn]: HashFn[T[A]]
  }
}
