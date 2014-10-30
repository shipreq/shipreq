package shipreq.base

import scala.annotation.elidable

package object prop {

  implicit class AnyExt[A](val a: A) extends AnyVal {

    @elidable(elidable.ASSERTION)
    def assertSatisfies(p: Prop[A]): Unit = p.assert1(a)
  }
}
