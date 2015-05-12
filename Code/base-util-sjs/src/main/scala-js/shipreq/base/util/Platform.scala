package shipreq.base.util

import scala.collection.mutable.AnyRefMap
import scalajs.js.UndefOr

// ================
// ====        ====
// ====   JS   ====
// ====        ====
// ================

object Platform {

  def memo[A <: AnyRef : UnivEq, B](f: A => B): A => B = {
    val cache = new AnyRefMap[A, B](128)
    a => cache.getOrElseUpdate(a, f(a))
  }

  class StreamUExt[A](val _a: Stream[UndefOr[A]]) extends AnyVal {
    def jsDefined: Stream[A] =
      _a.filter(_.isDefined).map(_.get)
  }

  abstract class ScalaExt {
    implicit def StreamUExt[A](s: Stream[UndefOr[A]]) =
      new StreamUExt[A](s)
  }
}
