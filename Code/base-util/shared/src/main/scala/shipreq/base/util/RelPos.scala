package shipreq.base.util

import cats.Eq

object RelPos {

  /**
   * @return The element immediately following the given subject.
   */
  def get[A: Eq](v: Vector[A], subj: A): RelPos[A] = {
    val i = v.indexWhere(subj === _)
    if (i >= 0 && (i + 1) < v.length) Some(v(i + 1)) else None
  }

  /** Inserts `subj` immediately before a given element, or appends if `before` is None. */
  def set[A: Eq](v: Vector[A], subj: A, pos: RelPos[A]): Vector[A] =
    pos match {
      case Some(b) =>
        v.foldLeft(Vector.empty[A])((q, e) => {
          val q2 = if (e === b) q :+ subj else q
          if (e === subj) q2 else q2 :+ e
        })
      case None =>
        v.filterNot(_ === subj) :+ subj
    }
}
