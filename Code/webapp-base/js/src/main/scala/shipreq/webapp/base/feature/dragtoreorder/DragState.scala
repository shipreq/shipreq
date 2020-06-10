package shipreq.webapp.base.feature.dragtoreorder

import japgolly.univeq._

private[dragtoreorder] final case class DragState[A](items       : Vector[A],
                                                     dragSource  : Int,
                                                     dragLoc     : DragLoc,
                                                     currentOrder: Vector[Int]) {

  override def toString = s"DragState($dragSource, $dragLoc, $currentOrder)"

  def originalOrder = items.indices.toVector
  def dragSourceItem = items(dragSource)

  def orderWithoutTombstone: Vector[Int] =
    dragLoc match {
      case DragLoc.InParent
         | DragLoc.InChild(_) => currentOrder
      case DragLoc.Outside    => currentOrder.filterNot(_ ==* dragSource)
    }
}

