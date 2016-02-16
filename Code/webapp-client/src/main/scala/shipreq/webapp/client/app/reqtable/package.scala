package shipreq.webapp.client.app

import japgolly.scalajs.react._, vdom.prefix_<^._
import japgolly.scalajs.react.extra._
import shipreq.base.util.{Intersection, NonEmptyVector, UnivEq}
import shipreq.webapp.base.data.{Live, Dead}
import shipreq.webapp.client.lib.DataReusability._
import shipreq.webapp.client.feature._
import ContentEditorFeature.EditFieldKey

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

  @inline def shouldComponentUpdate[P: Reusability, S: Reusability, B, N <: TopNode] =
    shipreq.webapp.client.app.shouldComponentUpdate[P, S, B, N]
    // Reusability.shouldComponentUpdateWithOverlay[P, S, B, N]

  // -----------------------------------------------------------------------------------------------

  val ColumnToEditFieldKey = Intersection[Column, EditFieldKey] {
    case Column.ReqType               => Some(EditFieldKey.ReqType        )
    case Column.Code                  => Some(EditFieldKey.Code           )
    case Column.Title                 => Some(EditFieldKey.Title          )
    case Column.Tags                  => Some(EditFieldKey.Tags           )
    case Column.ImplicationSrc        => Some(EditFieldKey.ImplicationSrc )
    case Column.ImplicationTgt        => Some(EditFieldKey.ImplicationTgt )
    case Column.CustomField(id, Live) => Some(EditFieldKey.CustomField(id))
    case Column.Pubid
       | Column.DeletionReason
       | Column.CustomField(_, Dead)  => None
  } {
    case EditFieldKey.ReqType         => Some(Column.ReqType              )
    case EditFieldKey.Code            => Some(Column.Code                 )
    case EditFieldKey.Title           => Some(Column.Title                )
    case EditFieldKey.Tags            => Some(Column.Tags                 )
    case EditFieldKey.ImplicationSrc  => Some(Column.ImplicationSrc       )
    case EditFieldKey.ImplicationTgt  => Some(Column.ImplicationTgt       )
    case EditFieldKey.CustomField(id) => Some(Column.CustomField(id, Live))
  }

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
