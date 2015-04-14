package shipreq.base.util

import scalaz.Equal

/**
 * I can't think of what else to call this.
 * This isn't exactly FRP but it's similar.
 * In FRP, changing a node updates all of its transitive children.
 * In this, changing a node only changes the node, but when any of its transitive children are next asked for a value,
 * they will update themselves.
 */
sealed abstract class Rx[A] {

  def rev: Int

  def value(): A

  def peek: A

  final def valueSince(r: Int): Option[A] = {
    val v = value()
    if (rev != r)
      Some(v)
    else
      None
  }

  // def map2[B](f: (A, B, A) => Option[B]): Rx.Map[A, B] =

  def map[B](f: A => B): Rx[B] =
    new Rx.Map(this, f)

  def flatMap[B](f: A => Rx[B]): Rx[B] =
    new Rx.FlatMap(this, f)

  //override def toString = value().toString
}

object Rx {

  sealed abstract class Root[A] extends Rx[A] {
    protected val ignoreChange: (A, A) => Boolean

    protected var _rev = 0
    protected var _value: A

    override final def rev = _rev

    override final def peek = _value

    protected def setMaybe(a: A): Unit =
      if (!ignoreChange(_value, a)) {
        _rev += 1
        _value = a
      }
  }

  final class Var[A](initialValue: A, protected val ignoreChange: (A, A) => Boolean) extends Root[A] {
    override protected var _value = initialValue

    override def value() = _value

    def set(a: A): Unit =
      setMaybe(a)
  }

  /** M = Manual refresh */
  final class ThunkM[A](next: () => A, protected val ignoreChange: (A, A) => Boolean) extends Root[A] {
    override protected var _value = next()

    override def value() = _value

    def refresh(): Unit =
      setMaybe(next())
  }

  /** A = Auto refresh */
  final class ThunkA[A](next: () => A, protected val ignoreChange: (A, A) => Boolean) extends Root[A] {
    override protected var _value = next()

    override def value() = {
      setMaybe(next())
      _value
    }
  }

  final class Map[A, B](xa: Rx[A], f: A => B) extends Rx[B] {
    private var _value = f(xa.value())
    private var _revA  = xa.rev

    override def rev  = _revA
    override def peek = _value

    override def map[C](g: B => C): Rx[C] =
      new Map(xa, g compose f)

    override def flatMap[C](g: B => Rx[C]): Rx[C] =
      new Rx.FlatMap(xa, g compose f)

    override def value(): B = {
      xa.valueSince(_revA).foreach { a =>
        _value = f(a)
        _revA = xa.rev
      }
      _value
    }
  }

  final class FlatMap[A, B](xa: Rx[A], f: A => Rx[B]) extends Rx[B] {
    private var _value = f(xa.value())
    private var _revA  = xa.rev

    override def rev  = _revA + _value.rev
    override def peek = _value.peek

    override def map[C](g: B => C): Rx[C] =
      new FlatMap(xa, f(_: A) map g)

    override def value(): B = {
      xa.valueSince(_revA).foreach { a =>
        _value = f(a)
        _revA = xa.rev
      }
      _value.value()
    }
  }

  // ===================================================================================================================

  object AutoValue {
    @inline implicit def autoRxValue[A](x: Rx[A]): A = x.value()
  }

  @inline def refresh(xs: ThunkM[_]*): Unit =
    xs.foreach(_.refresh())

  // ===================================================================================================================

  final class NeedIgnoreChange[A, R <: Rx[A]](val f: ((A, A) => Boolean) => R) extends AnyVal {
    def noReuse                          : R = f((_, _) => false)
    def reuse(g: (A, A) => Boolean)      : R = f(g)
    def reuseR(implicit ev: A <:< AnyRef): R = f((a, b) => ev(a) eq ev(b))
    def reuseE(implicit e: Equal[A])     : R = f(e.equal)
  }

  def apply[A](a: A) =
    new NeedIgnoreChange[A, Var[A]](new Var(a, _))

  def thunkM[A](f: => A) =
    new NeedIgnoreChange[A, ThunkM[A]](new ThunkM(() => f, _))

  def thunkA[A](f: => A) =
    new NeedIgnoreChange[A, ThunkA[A]](new ThunkA(() => f, _))

  def apply2[A, B, Z](xa: Rx[A], xb: Rx[B])(f: (A, B) => Z): Rx[Z] =
    for {a ← xa; b ← xb} yield f(a,b)

  def apply3[A, B, C, Z](xa: Rx[A], xb: Rx[B], xc: Rx[C])(f: (A, B, C) => Z): Rx[Z] =
    for {a ← xa; b ← xb; c ← xc} yield f(a,b,c)

  def apply4[A, B, C, D, Z](xa: Rx[A], xb: Rx[B], xc: Rx[C], xd: Rx[D])(f: (A, B, C, D) => Z): Rx[Z] =
    for {a ← xa; b ← xb; c ← xc; d ← xd} yield f(a,b,c,d)
}
