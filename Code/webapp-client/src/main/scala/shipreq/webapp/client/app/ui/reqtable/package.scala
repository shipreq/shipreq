package shipreq.webapp.client.app.ui

import japgolly.scalajs.react._, vdom.prefix_<^._
import japgolly.scalajs.react.extra._
import monocle.Lens
import shipreq.base.util.UnivEq
import shipreq.webapp.client.lib.TCB
import shipreq.webapp.client.lib.ui.feature.AsyncActionFeature

/**
 * Requirements Table.
 * "Common Req View & Editor" in the prototype.
 *
 * An Excel-like table for reading and editing requirements.
 */
package object reqtable {

  type Rows = Vector[Row]
  implicit val reusabilityRows: Reusability[Rows] = Reusability.byRef // Each row will be checked anyway

  type RowSelection        = Selection[Row.SourceId]
  type RowSelectionVisible = Selection.LegalWithUpdateFn[Row.SourceId]

  type CallServer[-I] = (I, TCB.Success, String => TCB.Failure) => Callback
  implicit def callServerReusability[I] = Reusability.byRef[CallServer[I]] // All are vals in ReqTable

  @inline def shouldComponentUpdate[P: Reusability, S: Reusability, B, N <: TopNode] =
    shipreq.webapp.client.app.ui.shouldComponentUpdate[P, S, B, N]
    // Reusability.shouldComponentUpdateWithOverlay[P, S, B, N]

  sealed trait FocusId
  object FocusId {
    case class AtCell(row: Row.SourceId, col: Column) extends FocusId

    implicit def equality: UnivEq[FocusId] = UnivEq.deriveAuto
  }

  object EditState {
    type R = Row.SourceId
    type C = Column
    type Table = Map[R, AtRow]
    type AtRow = Map[C, CellEditor]

    def empty: Table =
      UnivEq.emptyMap

    import monocle.function.At.at
    import monocle.std.map.atMap

    def getRow(t: Table, r: R): AtRow =
      t.getOrElse(r, UnivEq.emptyMap)

    def atRow(r: R): Lens[Table, AtRow] =
      Lens[Table, AtRow](
        s => getRow(s, r))(
        s => m => if (s.isEmpty) m - r else m.updated(r, s))

    def atCell(r: R, c: C): Lens[Table, Option[CellEditor]] =
      atRow(r) ^|-> at(c)
  }

  val AsyncState = AsyncActionFeature.Table.Fix[Row.SourceId, Column, String]

  def renderAsyncState(s: AsyncState.Status): ReactTag =
    s match {
      case AsyncActionFeature.Locked =>
        AsyncActionFeature.renderLocked

      case f: AsyncActionFeature.Failed[AsyncState.Failure] =>
        <.div(f.failure, f.retryButton, f.resumeEditButton)
    }
}
