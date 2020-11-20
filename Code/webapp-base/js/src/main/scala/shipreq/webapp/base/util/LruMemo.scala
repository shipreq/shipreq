package shipreq.webapp.base.util

import japgolly.scalajs.react.Reusability
import scala.runtime.AbstractFunction1

final class LruMemo[@specialized(Int) A, B](f     : A => B,
                                            isSame: (A, A) => Boolean,
                                            state : LruMemo.State[A, B]) extends AbstractFunction1[A, B] {

  override def apply(a: A): B =
    state.get(a, isSame, f)
}

object LruMemo {

  def byUnivEq[A, B](f: A => B, maxSize: Int)(implicit e: UnivEq[A]): LruMemo[A, B] =
    apply(f, maxSize, e.univEq)

  def byReusability[A, B](f: A => B, maxSize: Int)(implicit r: Reusability[A]): LruMemo[A, B] =
    apply(f, maxSize, r.test)

  private def apply[A, B](f: A => B, maxSize: Int, isSame: (A, A) => Boolean): LruMemo[A, B] =
    new LruMemo(f, isSame, newState(maxSize))

  // ===================================================================================================================

  object ExternalFn {

    def byUnivEq[K, V](maxSize: Int)(implicit e: UnivEq[K]): ExternalFn[K, V] =
      apply(maxSize, e.univEq)

    def byReusability[A, B](maxSize: Int)(implicit r: Reusability[A]): ExternalFn[A, B] =
      apply(maxSize, r.test)

    private def apply[A, B](maxSize: Int, isSame: (A, A) => Boolean): ExternalFn[A, B] =
      new ExternalFn(isSame, newState(maxSize))
  }

  final class ExternalFn[@specialized(Int) K, V](isSame: (K, K) => Boolean,
                                                 state: LruMemo.State[K, V]) {

    def get(a: K)(f: K => V): V =
      state.get(a, isSame, f)
  }

  // ===================================================================================================================

  private def newState[@specialized(Int) K, V](maxSize: Int): State[K, V] = {
    assert(maxSize > 0)
    new State(maxSize, new Array(maxSize))
  }

  private final class State[@specialized(Int) K, V](val maxSize: Int,
                                                    val cache: Array[Result[K, V]]) {
    var time = 0
    var size = 0

    def get(key: K, isSame: (K, K) => Boolean, f: K => V): V = {
      val t = time
      time += 1

      // Check if acceptable result exists
      var i = 0
      var minTime = -1
      var minIdx = -1
      while (i < size) {
        val r = cache(i)
        if (isSame(r.key, key)) {
          r.lastAccessed = t
          return r.value
        }

        if (minIdx == -1 || r.lastAccessed < minTime) {
          minIdx = i
          minTime = r.lastAccessed
        }

        i += 1
      }

      // Not found. Calculate.
      val b = f(key)

      // Add to cache
      val r = new Result(key, b, t)
      if (size < maxSize) {
        cache(size) = r
        size += 1
      } else {
        cache(minIdx) = r
      }

      b
    }
  }

  private final class Result[@specialized(Int) K, V](val key: K,
                                                     val value: V,
                                                     var lastAccessed: Int)

}