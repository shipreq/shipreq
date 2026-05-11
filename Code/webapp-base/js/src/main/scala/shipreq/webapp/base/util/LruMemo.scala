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

  def apply[A, B](f: A => B, maxSize: Int): Dsl[A, B] =
    new Dsl(f, maxSize)

  final class Dsl[@specialized(Int) A, B](f: A => B, maxSize: Int) {

    def apply(isSame: (A, A) => Boolean): LruMemo[A, B] =
      new LruMemo(f, isSame, newState(maxSize))

    def by[K](k: A => K): DslBy[A, B, K] =
      new DslBy(f, k, maxSize)

    def byUnivEq(implicit e: UnivEq[A]): LruMemo[A, B] =
      apply(e.univEq)

    def byReusability(implicit r: Reusability[A]): LruMemo[A, B] =
      apply(r.test)
  }

  final class DslBy[@specialized(Int) A, B, @specialized(Int) K](f: A => B, k: A => K, maxSize: Int) {

    def apply(isSame: (K, K) => Boolean): LruMemo[A, B] =
      new LruMemo(f, (x, y) => isSame(k(x), k(y)), newState(maxSize))

    def byUnivEq(implicit e: UnivEq[K]): LruMemo[A, B] =
      apply(e.univEq)

    def byReusability(implicit r: Reusability[K]): LruMemo[A, B] =
      apply(r.test)
  }

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

    def get(k: K)(f: K => V): V =
      state.get(k, isSame, f)

    def getOrElsePut(k: K)(v: => V): V =
      get(k)(_ => v)

//    def getOrThrow(k: K): V =
//      get(k)(_ => throw new RuntimeException("Value not found for " + k))

    def foreachKey(f: K => Unit): Unit =
      state.foreachKey(f)

    /** By "IgnoreAccess" I mean that the lastAccessed field of these values won't be updated. */
    def foreachValueIgnoreAccess(f: V => Unit): Unit =
      state.foreachValueIgnoreAccess(f)

    def duplicate(): ExternalFn[K, V] =
      new ExternalFn(isSame, state.duplicate())
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

    def duplicate(): State[K, V] = {
      val m = new State[K, V](maxSize, cache.clone())
      m.time = time
      m.size = size
      m
    }

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

    def foreachKey(f: K => Unit): Unit =
      foreach(r => f(r.key))

    def foreachValueIgnoreAccess(f: V => Unit): Unit =
      foreach(r => f(r.value))

    private def foreach(f: Result[K, V] => Unit): Unit = {
      var i = 0
      while (i < size) {
        val r = cache(i)
        f(r)
        i += 1
      }
    }

  }

  private final class Result[@specialized(Int) K, V](val key: K,
                                                     val value: V,
                                                     var lastAccessed: Int)

}