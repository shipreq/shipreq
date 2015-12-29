package shipreq.webapp.client.app.ui

import japgolly.scalajs.react._, vdom.prefix_<^._
import japgolly.scalajs.react.extra._
import monocle.Lens
import monocle.function.At.at
import monocle.std.map.atMap
import shipreq.base.util.{NonEmptyVector, UnivEq}
import shipreq.webapp.client.data.DataReusability._
import shipreq.webapp.client.lib.TCB
import shipreq.webapp.client.lib.ui.feature._

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

  implicit def reusabilityCallServer[I]: Reusability[CallServer[I]] =
    Reusability.byRef // All are vals in ReqTable

  implicit def reusabilityCR: Reusability[ColumnRenderer] =
    Reusability.byRef // TODO This is a problem

  implicit val reusabilityCRs: Reusability[NonEmptyVector[ColumnRenderer]] =
    Reusability.byRef || reusabilityNonEmptyVector

  implicit val reusabilityCs: Reusability[NonEmptyVector[Column]] =
    Reusability.byRef || reusabilityNonEmptyVector

  @inline def shouldComponentUpdate[P: Reusability, S: Reusability, B, N <: TopNode] =
    shipreq.webapp.client.app.ui.shouldComponentUpdate[P, S, B, N]
    // Reusability.shouldComponentUpdateWithOverlay[P, S, B, N]

  // -----------------------------------------------------------------------------------------------

  object EditState {
    type R     = Row.SourceId
    type C     = Column
    type Table = Map[R, AtRow]
    type AtRow = Map[C, CellEditor]

    def empty: Table =
      UnivEq.emptyMap

    def getRow(t: Table, r: R): AtRow =
      t.getOrElse(r, UnivEq.emptyMap)

    def atRow(r: R): Lens[Table, AtRow] =
      Lens[Table, AtRow](
        s => getRow(s, r))(
        s => m => if (s.isEmpty) m - r else m.updated(r, s))

    def atCell(r: R, c: C): Lens[Table, Option[CellEditor]] =
      atRow(r) ^|-> at(c)
  }

  implicit def reusabilityCE: Reusability[CellEditor] =
    Reusability.never // ∵ renderCB changes with state (i.e. the Pxs and reading of PreviewFeature state)

  implicit def reusabilityCEs: Reusability[CellEditors] =
    Reusability.byRef

  implicit def reusabilityEditStateTable: Reusability[EditState.Table] =
    // Contents are effectively mutable (see reusabilityCE comment)
    Reusability.fn((a, b) => a.isEmpty && b.isEmpty)

  implicit def reusabilityEditStateAtRow: Reusability[EditState.AtRow] =
    // Contents are effectively mutable (see reusabilityCE comment)
    Reusability.fn((a, b) => a.isEmpty && b.isEmpty)

  // -----------------------------------------------------------------------------------------------

  val AsyncState = AsyncActionFeature.Table.Fix[Row.SourceId, Column, String]

  def renderAsyncState(s: AsyncState.Status): ReactTag =
    s match {
      case AsyncActionFeature.Locked =>
        AsyncActionFeature.renderLocked

      case f: AsyncActionFeature.Failed[AsyncState.Failure] =>
        <.div(f.failure, f.retryButton, f.resumeEditButton)
    }

  // -----------------------------------------------------------------------------------------------

  sealed trait FocusId
  object FocusId {
    case class AtCell(row: Row.SourceId, col: Column) extends FocusId
    case class InCI(typ: CreationInterface.Type, col: Column) extends FocusId
    implicit def equality: UnivEq[FocusId] = UnivEq.deriveAuto
  }

  val Preview = PreviewFeature.FixKey[FocusId]
}
