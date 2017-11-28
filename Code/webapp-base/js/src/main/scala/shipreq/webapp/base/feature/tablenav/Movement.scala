package shipreq.webapp.base.feature.tablenav

import shipreq.base.util.Util

sealed trait Axis
object Axis {
  case object UpDown extends Axis
  case object LeftRight extends Axis
}

sealed abstract class Movement(val adjustIndex: Int => Int) {
  final def apply[A](as: IndexedSeq[A], currentIndex: Int): A = {
    val j = Util.fitCollectionIndex(adjustIndex(currentIndex), as.length)
    as(j)
  }
}

object Movement {
  case object Prev extends Movement(_ - 1)
  case object Next extends Movement(_ + 1)
  case object Head extends Movement(_ => 0)
  case object Last extends Movement(_ => -1)
}
