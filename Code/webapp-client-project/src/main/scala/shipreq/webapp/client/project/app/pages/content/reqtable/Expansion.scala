package shipreq.webapp.client.project.app.pages.content.reqtable

import japgolly.univeq.UnivEq
import monocle.Lens
import monocle.macros.GenLens
import scalaz.{Equal, Semigroup}
import shipreq.base.util._

final case class Expansion[+A](values: Vector[A], original: Vector[A]) {
  lazy val result: Vector[A] =
    if (values eq original)
      values
    else
      // Performance looks bad here but given overhead of a better CS-theoretic implementation this is actually likely
      // to be faster given the even the high watermark of quantities we can expect here.
      values ++ original.filterNot(values.contains)
}

object Expansion {

  def empty: Expansion[Nothing] =
    apply(Vector.empty, Vector.empty)

  implicit def univEq[A: UnivEq]: UnivEq[Expansion[A]] = UnivEq.derive

  implicit def semigroup[A: Equal]: Semigroup[Expansion[A]] = {
    new Semigroup[Expansion[A]] {
      override def append(x: Expansion[A], yy: => Expansion[A]) = {
        val y = yy
        Expansion(
          values   = Util.vectorConcatDistinct(x.values, y.values),
          original = Util.vectorConcatDistinct(x.original, y.original), // ok because (x.original eq y.original)
        )
      }
    }
  }

  def values[A]: Lens[Expansion[A], Vector[A]] =
    GenLens[Expansion[A]](_.values)
}
