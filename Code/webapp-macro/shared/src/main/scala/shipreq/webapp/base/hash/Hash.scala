package shipreq.webapp.base.hash

/**
 * Provides a 32-bit hash of a value inhabiting an invariant type.
 */
class Hash[A](val hash: A => Int) extends AnyVal {

  @inline def narrow[B <: A]: Hash[B] =
    Hash.fn(hash)

  def cmap[B](f: B => A): Hash[B] =
    Hash.fn(hash compose f)
}

// =====================================================================================================================

object Hash {

  @inline def apply[A](implicit h: Hash[A]): Hash[A] = h

  @inline def fn[A](f: A => Int): Hash[A] =
    new Hash(f)

  @inline def by[A, B: Hash](f: A => B): Hash[A] =
    Hash[B].cmap(f)

  @inline implicit class HashableValueOps[A](val _a: A) extends AnyVal {
    def hash(implicit h: Hash[A]): Int = h hash _a
  }

  @inline def const[A](hash: Int): Hash[A] =
    fn[A](_ => hash)

  def constOf[A: Hash, B](a: A): Hash[B] =
    const[B](a.hash)

  @inline def internal[A]: Hash[A] =
    new Hash(_.##)

  final val UnsupportedValue: Int =
    0xffffffff

  @inline def unsupported[A]: Hash[A] =
    const(UnsupportedValue)

  def lazily[A](hash: => Hash[A]): Hash[A] = {
    lazy val h = hash
    fn(h.hash(_))
  }

  // -------------------------------------------------------------------------------------------------------------------

  trait Algorithm extends HashMacros {
    implicit val hashBoolean                  : Hash[Boolean]
    implicit val hashChar                     : Hash[Char]
    implicit val hashInt                      : Hash[Int]
    implicit val hashLong                     : Hash[Long]
    implicit val hashString                   : Hash[String]
    implicit def hashPair   [A: Hash, B: Hash]: Hash[(A, B)]
    implicit def hashMap    [K: Hash, V: Hash]: Hash[Map[K, V]]
    implicit def hashSet    [A: Hash]         : Hash[Set[A]]
    implicit def hashList   [A: Hash]         : Hash[List[A]]
    implicit def hashVector [A: Hash]         : Hash[Vector[A]]

    def hashUnordered[T[x] <: TraversableOnce[x], A: Hash]: Hash[T[A]]
  }
}

