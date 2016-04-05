package shipreq.base.util

import japgolly.univeq.UnivEq
import scala.collection.mutable
import scalajs.js.UndefOr

// ================
// ====        ====
// ====   JS   ====
// ====        ====
// ================

object Platform extends PlatformShared {

  override def memo[A: UnivEq, B](f: A => B): A => B = {
    // val cache = new mutable.AnyRefMap[A, B](128)
    val cache = new mutable.HashMap[A, B]()
    a => cache.getOrElseUpdate(a, f(a))
  }

  override def looseMemo[A: UnivEq, B](): LooseMemo[A, B] = {
    val cache = new mutable.HashMap[A, B]()
    cache.getOrElseUpdate
  }

  override def memoInt[A](f: Int => A): Int => A =
    memo(f)

  class StreamUExt[A](val _a: Stream[UndefOr[A]]) extends AnyVal {
    def jsDefined: Stream[A] =
      _a.filter(_.isDefined).map(_.get)
  }

  abstract class ScalaExt {
    implicit def StreamUExt[A](s: Stream[UndefOr[A]]) =
      new StreamUExt[A](s)
  }
}
