package shipreq.base.util.storecache

import scalaz.Applicative

final class LazyVal[A](create: () => A) {

  @volatile private[this] var createFn = create

  lazy val value: A = {
    val a = createFn()
    createFn = null
    a
  }

  def map[B](f: A => B): LazyVal[B] =
    LazyVal(f(value))

  def flatMap[B](f: A => LazyVal[B]): LazyVal[B] =
    LazyVal(f(value).value)
}

object LazyVal {
  def apply[A](a: => A): LazyVal[A] =
    new LazyVal(() => a)

  implicit val scalazInstance: Applicative[LazyVal] =
    new Applicative[LazyVal] {
      override def point[A](a: => A): LazyVal[A] =
        LazyVal(a)

      override def ap[A, B](_fa: => LazyVal[A])(_ff: => LazyVal[A => B]): LazyVal[B] = {
        val fa = _fa
        val ff = _ff
        for {
          a <- fa
          f <- ff
        } yield f(a)
      }
    }
}