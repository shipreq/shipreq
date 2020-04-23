package shipreq.base.util

import japgolly.univeq.UnivEq
import shipreq.base.util.EitherState.ScalazTrampoline._

final class LazyVal[A](create: Trampoline[A]) {

  private var _create = create

  private[LazyVal] val trampoline: Trampoline[A] =
    Trampoline.suspend {
      val v = _value
      if (v.isEmpty)
        _create.map { a =>
          synchronized {
            __value = Some(a)
            _create = Trampoline.pure(a)
          }
          a
        }
      else
        _create
    }

  private[this] var __value = Option.empty[A]

  private[this] def _value: Option[A] =
    __value.orElse(synchronized(__value))

  def value: A =
    _value.getOrElse(Trampoline.run(trampoline))

  def map[B](f: A => B): LazyVal[B] =
    // new LazyVal(trampoline.map(f))
    new LazyVal(trampoline.flatMap { a =>
      Trampoline.delay(f(a))
    })

  def flatMap[B](f: A => LazyVal[B]): LazyVal[B] =
    new LazyVal(trampoline.flatMap { a =>
      Trampoline.suspend(f(a).trampoline)
    })

  override def hashCode =
    trampoline.hashCode

  override def toString =
    s"LazyVal(${_value.fold("?")(_.toString)})"

  override def equals(obj: Any): Boolean = obj match {
    case l: LazyVal[_] => (this eq l) || (value == l.value)
    case _             => false
  }
}

object LazyVal {
  def apply[A](a: => A): LazyVal[A] =
    new LazyVal(Trampoline.delay(a))

  def pure[A](a: A): LazyVal[A] =
    new LazyVal(Trampoline.pure(a))

  def suspend[A](l: => LazyVal[A]): LazyVal[A] =
    new LazyVal(Trampoline.suspend(l.trampoline))

  private[this] val False = LazyVal.pure(false)

  implicit def univEq[A]: UnivEq[A] =
    UnivEq.force

  def exists[A](ls: LazyVal[A]*)(f: A => Boolean): LazyVal[Boolean] =
    ls.foldLeft(False)((q, n) =>
      q.flatMap { found =>
        if (found)
          q
        else
          n.map(f)
      }
    )
}