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

    def initStrict(init: I): Cache

    def nextStrict(prev: Cache, i: I): Cache

    final def nextStrict(prev: Option[Cache], i: I): Cache =
      prev.fold(initStrict(i))(nextStrict(_, i))

    def initLazy(init: => I): Cache

    def nextLazyFull(prev: Cache, i: => I): Next[Cache]

    final def nextLazy(prev: Cache, i: => I): Cache =
      nextLazyFull(prev, i).value

    final def nextLazy(prev: Option[Cache], i: => I): Cache =
      prev.fold(initLazy(i))(nextLazy(_, i))

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
