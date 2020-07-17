package shipreq.webapp.base.feature.dragtoreorder

import japgolly.microlibs.adt_macros.AdtMacros
sealed trait Status

object Status {

  /** Item isn't being dragged. */
  case object Normal extends Status

  /** Item is being dragged. You'd normally draw it with a dashed border, or `visibility: hidden`. */
  case object DragSource extends Status

  /**
   * Item has been dragged out to indicate deletion.
   *
   * You must still render a node for this, do not omit it because it will prevent expected events from firing.
   * Use `display: none` instead.
   */
  case object Tombstone extends Status

  implicit def univEqStatus: UnivEq[Status] = UnivEq.derive

  val allValues = AdtMacros.adtValues[Status]
}