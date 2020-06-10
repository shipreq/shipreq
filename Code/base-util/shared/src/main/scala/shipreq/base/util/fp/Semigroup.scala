package shipreq.base.util.fp

trait Semigroup[@specialized(Int) A] {
  def append(a: A, b: A): A
}

object Semigroup {

  final class Ops[A](private val self: A) extends AnyVal {
    @inline final def |+|(a: A)(implicit s: Semigroup[A]): A =
      s.append(self, a)
  }

  object Syntax {
    implicit def semigroupOps[@specialized(Int) A](a: A): Ops[A] =
      new Ops(a)
  }

}
