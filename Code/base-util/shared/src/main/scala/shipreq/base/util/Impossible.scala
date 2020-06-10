package shipreq.base.util

import japgolly.univeq.UnivEq
import scala.annotation.elidable

/** Unfortunately using `Nothing` results in Scala 2 behaving differently and having lots of problems wrt implicit
  * resolution and type inference.
  *
  * This is an alternative.
  */
sealed trait Impossible {

  @elidable(elidable.ALL)
  final def impossible: Nothing =
    throw new RuntimeException("Impossible.impossible called!")
}

object Impossible {
  @inline implicit def univEq: UnivEq[Impossible] = UnivEq.force
}