package shipreq.base.util.fp

trait Monoid[@specialized(Int) A] extends Semigroup[A] {
  def zero: A
}

object Monoid {

  object IntAddition extends Monoid[Int] {
    override final val zero = 0
    override def append(a: Int, b: Int) = a + b
  }

  private def set[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      override def zero = Set.empty[A]
      override def append(a: Set[A], b: Set[A]) = a ++ b
    }

  private def vector[A]: Monoid[Vector[A]] =
    new Monoid[Vector[A]] {
      override def zero = Vector.empty[A]
      override def append(a: Vector[A], b: Vector[A]) = a ++ b
    }

  object Implicits {

    @inline implicit def monoidIntAddition: Monoid[Int] =
      IntAddition

    private[this] val _vector = vector[Any]

    implicit def monoidVector[A]: Monoid[Vector[A]] =
      _vector.asInstanceOf[Monoid[Vector[A]]]

    private[this] val _set = set[Any]

    implicit def monoidSet[A]: Monoid[Set[A]] =
      _set.asInstanceOf[Monoid[Set[A]]]
  }
}