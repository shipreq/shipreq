package shipreq.webapp.base.util

import cats.Eq
import japgolly.microlibs.stdlib_ext.StdlibExt._

object Reorder {

  def usingEqual[@specialized(Int) A](from: A, to: A)(as: Vector[A])(implicit e: Eq[A]): Vector[A] =
    apply(from, to, as)(e.eqv)

  @nowarn("cat=unused")
  def usingUnivEq[@specialized(Int) A](from: A, to: A)(as: Vector[A])(implicit e: UnivEq[A]): Vector[A] =
    apply(from, to, as)(_ == _)

  def apply[@specialized(Int) A, @specialized(Int) B](from: A, to: A, bs: Vector[B])(equal: (A, B) => Boolean): Vector[B] = {
    val tmp = bs.iterableFactory.newBuilder[B]
    var putLater = -1
    var fromB: Option[B] = None
    var i = 0
    val it = bs.iterator
    @tailrec def go(): Boolean = {
      if (it.isEmpty)
        false
      else {
        val b = it.next()
        if (fromB.isEmpty && equal(from, b)) {
          if (equal(to, b))
            true // nothing to do
          else {
            fromB = Some(b)
            go()
          }
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
          go()
        }
      }
    }
    val abort = go()
    if (abort)
      bs
    else {
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

}
