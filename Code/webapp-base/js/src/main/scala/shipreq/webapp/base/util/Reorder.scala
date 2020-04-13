package shipreq.webapp.base.util

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq.UnivEq
import scalaz.Equal

object Reorder {

  def usingEqual[@specialized(Int) A](from: A, to: A)(as: Vector[A])(implicit e: Equal[A]): Vector[A] =
    apply(from, to, as)(e.equal)

  def usingUnivEq[@specialized(Int) A](from: A, to: A)(as: Vector[A])(implicit e: UnivEq[A]): Vector[A] = {
    val _ = e
    apply(from, to, as)(_ == _)
  }

  def apply[@specialized(Int) A, @specialized(Int) B](from: A, to: A, bs: Vector[B])(equal: (A, B) => Boolean): Vector[B] = {
    val tmp = bs.iterableFactory.newBuilder[B]
    var putLater = -1
    var fromB: Option[B] = None
    var i = 0
    bs.foreach { b =>
      if (fromB.isEmpty && equal(from, b)) {
        if (equal(to, b))
          return bs // nothing to do
        fromB = Some(b)
      } else {
        tmp += b
        if (equal(to, b)) {
          fromB match {
            case None =>
              putLater = i
              tmp += b // This is the correct b, we will replace this-1
            case Some(ins) =>
              tmp += ins
          }
          i += 1
        }
        i += 1
      }
    }
    val tmp2 = tmp.result()
    val result =
      if (putLater == -1)
        tmp2
      else fromB match {
        case Some(b) => tmp2.updated(putLater, b)
        case None    => tmp2.delete(putLater).getOrElse(tmp2)
      }
    assert(result.size == bs.size, s"DND Move failure.\nBefore: $bs\n After: $result")
    result
  }

}
