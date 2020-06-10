package shipreq.webapp.base.feature.dragtoreorder

import japgolly.univeq.UnivEq

/** Where the drag cursor is currently located. */
private[dragtoreorder] sealed trait DragLoc

private[dragtoreorder] object DragLoc {

  case object Outside              extends DragLoc
  case object InParent             extends DragLoc
  final case class InChild(i: Int) extends DragLoc

  implicit def univEq: UnivEq[DragLoc] = UnivEq.derive
}
