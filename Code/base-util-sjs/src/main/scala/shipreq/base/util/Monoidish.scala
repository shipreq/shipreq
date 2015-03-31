package shipreq.base.util

trait Monoidish[M[_]] {
  def empty [A]                  : M[A]
  def add   [A](m: M[A], a: A)   : M[A]
  def append[A](m: M[A], n: M[A]): M[A]
}

object Monoidish {

  implicit val setInstance: Monoidish[Set] =
    new Monoidish[Set] {
      override def empty [A]                      : Set[A] = Set.empty 
      override def add   [A](m: Set[A], a: A)     : Set[A] = m + a
      override def append[A](m: Set[A], n: Set[A]): Set[A] = m ++ n
    }

  implicit val vectorInstance: Monoidish[Vector] =
    new Monoidish[Vector] {
      override def empty [A]                            : Vector[A] = Vector.empty
      override def add   [A](m: Vector[A], a: A)        : Vector[A] = m :+ a
      override def append[A](m: Vector[A], n: Vector[A]): Vector[A] = m ++ n
    }

  implicit val streamInstance: Monoidish[Stream] =
    new Monoidish[Stream] {
      override def empty [A]                            : Stream[A] = Stream.empty
      override def add   [A](m: Stream[A], a: A)        : Stream[A] = m append Stream.cons(a, Stream.empty)
      override def append[A](m: Stream[A], n: Stream[A]): Stream[A] = m append n
    }
}