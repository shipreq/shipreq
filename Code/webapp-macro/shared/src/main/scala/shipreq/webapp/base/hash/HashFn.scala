package shipreq.webapp.base.hash

/**
 * Provides a 32-bit hash of a value inhabiting an invariant type.
 */
final class HashFn[@specialized(Int, Long, Char, Boolean) A](val hashFn: A => Int) /*extends AnyVal*/ {

  @inline def apply(a: A): Int =
    hashFn(a)

  @inline def narrow[@specialized(Int, Long, Char, Boolean) B <: A]: HashFn[B] =
    HashFn(hashFn)

  def contramap[@specialized(Int, Long, Char, Boolean) B](f: B => A): HashFn[B] =
    HashFn(hashFn compose f)
}

object HashFn {

  def apply[@specialized(Int, Long, Char, Boolean) A](f: A => Int): HashFn[A] =
    new HashFn(f)

  @inline def by[@specialized(Int, Long, Char, Boolean) A, @specialized(Int, Long, Char, Boolean) B](f: A => B)(implicit h: HashFn[B]): HashFn[A] =
    h contramap f

  @inline def const[@specialized(Int, Long, Char, Boolean) A](hash: Int): HashFn[A] =
    apply[A](_ => hash)

  def constByHashing[@specialized(Int, Long, Char, Boolean) A: HashFn, @specialized(Int, Long, Char, Boolean) B](a: A): HashFn[B] =
    const[B](Hash(a))

  def lazily[@specialized(Int, Long, Char, Boolean) A](hash: => HashFn[A]): HashFn[A] = {
    lazy val h = hash
    apply(h.hashFn(_))
  }

  @inline def internal[@specialized(Int, Long, Char, Boolean) A]: HashFn[A] =
    apply(_.##)
}
