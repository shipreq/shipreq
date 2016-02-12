package shipreq.webapp.client.app

import japgolly.scalajs.react._, vdom.prefix_<^._
import japgolly.scalajs.react.extra._
import shipreq.base.util.{NonEmptyVector, UnivEq}
import shipreq.webapp.client.lib.DataReusability._
import shipreq.webapp.client.feature._

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

  implicit def reusabilityCR: Reusability[ColumnRenderer] =
    Reusability.byRef // TODO This is a problem

  implicit val reusabilityCRs: Reusability[NonEmptyVector[ColumnRenderer]] =
    Reusability.byRef || reusabilityNonEmptyVector

  implicit val reusabilityCs: Reusability[NonEmptyVector[Column]] =
    Reusability.byRef || reusabilityNonEmptyVector

  implicit val reusabilityContentEditorFeature: Reusability[ContentEditorFeature.D2.Feature[Row, Column]] =
    Reusability.byRef

  implicit val reusabilityAsyncFeature: Reusability[AsyncActionFeature.D2.Feature[Row.SourceId, Column, String]] =
    Reusability.byRef

  @inline def shouldComponentUpdate[P: Reusability, S: Reusability, B, N <: TopNode] =
    shipreq.webapp.client.app.shouldComponentUpdate[P, S, B, N]
    // Reusability.shouldComponentUpdateWithOverlay[P, S, B, N]

  // -----------------------------------------------------------------------------------------------

  def renderAsyncState(s: AsyncActionFeature.Status[String]): ReactTag =
    s match {
      case AsyncActionFeature.Locked =>
        AsyncActionFeature.renderLocked

      case f: AsyncActionFeature.Failed[String] =>
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
