package shipreq.base.util.storecache

import shipreq.base.util.Identity

/** Version of the Store comonad built for caching.
  *
  * Basically, this is for when you want to have lazy vals in your class and want to retain the values (when possible)
  * after a `.copy`.
  */
trait StoreCache[I, A] {

  def value: A

  def contramap[J](f: J => I): StoreCache[J, A]

  def map[B](f: A => B): StoreCache[I, B]
}

object StoreCache extends StoreCacheBoilerplate {

  trait Logic[I, A] {
    type Cache <: StoreCache[I, A]

    def init(init: I): Cache

    def next(prev: Cache, i: I): Cache

    final def next(prev: Option[Cache], i: I): Cache =
      prev.fold(init(i))(next(_, i))

    def contramap[J](f: J => I): Logic[J, A]

    def map[B](f: A => B): Logic[I, B]
  }

  object Logic extends StoreCacheLogicBoilerplate {
    def apply[S: QuickEq, A](run: S => A): Logic1[S, S, A] =
      new Logic1[S, S, A](Identity.apply[S], run)

    @inline def fn1[S: QuickEq, A](run: S => A): Logic1[S, S, A] =
      apply(run)
  }
}
