package shipreq.base.util

import shipreq.base.util.Trampoline.Default._

sealed abstract class LazyVal[A] {
  def isEvaluated(): Boolean
  def value: A
  def map[B](f: A => B): LazyVal[B]
  def flatMap[B](f: A => LazyVal[B]): LazyVal[B]

  protected[util] val trampoline: Trampoline[A]

  final override def equals(obj: Any): Boolean = obj match {
    case l: LazyVal[_] => (this eq l) || (value == l.value)
    case _             => false
  }
}

object LazyVal {
  def apply[A](a: => A): LazyVal[A] =
    new Lazy(Trampoline.delay(a))

  def pure[A](a: A): LazyVal[A] =
    Pure(a)

  def suspend[A](l: => LazyVal[A]): LazyVal[A] =
    new Lazy(Trampoline.suspend(l.trampoline))

  val False = pure(false)
  val True = pure(true)

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

  // ===================================================================================================================

  private final case class Pure[A](value: A) extends LazyVal[A] {
    override def toString =
      s"LazyVal($value})"

    override def isEvaluated() =
      true

    override def map[B](f: A => B): LazyVal[B] =
      LazyVal(f(value))

    override def flatMap[B](f: A => LazyVal[B]): LazyVal[B] =
      f(value)

    override protected[util] val trampoline: Trampoline[A] =
      Trampoline.pure(value)
  }

  private final class Lazy[A](create: Trampoline[A]) extends LazyVal[A] {

    private var _create = create

    override protected[util] val trampoline: Trampoline[A] =
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

    override def isEvaluated(): Boolean =
      __value.isDefined || synchronized(__value).isDefined

    override def value: A =
      _value.getOrElse(Trampoline.run(trampoline))

    override def map[B](f: A => B): LazyVal[B] =
      // new LazyVal(trampoline.map(f))
      new Lazy(trampoline.flatMap { a =>
        Trampoline.delay(f(a))
      })

    override def flatMap[B](f: A => LazyVal[B]): LazyVal[B] =
      new Lazy(trampoline.flatMap { a =>
        Trampoline.suspend(f(a).trampoline)
      })

    override def hashCode =
      trampoline.hashCode

    override def toString =
      s"LazyVal(${_value.fold("?")(_.toString)})"
  }

}