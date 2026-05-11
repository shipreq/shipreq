package shipreq.base.util

final class MutableRef[@specialized(Boolean, Double) A] private[util] (initialValue: A) {

  override def hashCode =
    initialValue.##

  @elidable(elidable.INFO)
  override def toString =
    s"MutableRef($value)"

  var value = initialValue

  @inline def mod(f: A => A): Unit =
    value = f(value)
}

object MutableRef {
  def apply[A <: AnyRef](initialValue: A): MutableRef[A] =
    new MutableRef(initialValue)

  def option[A]: MutableRef[Option[A]] =
    apply(None)

  def double(initialValue: Double = 0): MutableRef[Double] =
    new MutableRef(initialValue)

  def boolean(initialValue: Boolean): MutableRef[Boolean] =
    new MutableRef(initialValue)
}
