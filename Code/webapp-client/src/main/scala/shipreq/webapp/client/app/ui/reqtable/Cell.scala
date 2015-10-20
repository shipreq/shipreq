package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.{Reusability, ~=>}
import shipreq.base.util.ScalaExt.EndoFn
import shipreq.base.util.UnivEq
import shipreq.webapp.client.app.ui.RemoteDataEditor

object Cell {

  type R = Row.SourceId
  type C = Column
  type S = RemoteDataEditor.State

  // Cell.State
  type State = Option[S]

  private type AllState = Map[R, RowState]

  sealed trait RowState {
    def apply(c: C): State = None
    def byColumns: Map[C, S] = UnivEq.emptyMap
    def allowEdit(c: C): Boolean
  }

  object RowState {
    case object Empty extends RowState {
      override def allowEdit(c: C) = true
    }

    case class WholeRow(state: S) extends RowState {
      override def allowEdit(c: C) = false
    }

    case class Cells(state: Map[C, S]) extends RowState {
      override def apply(c: C) = state get c
      override def byColumns: Map[C, S] = state
      override def allowEdit(c: C) = !state.contains(c)
    }
  }

  case class Loc(row: R, col: Option[C])

  implicit val locReusability: Reusability[Loc] = Reusability.caseClass

  final class TableState(m: AllState) {
    def apply(row: R): RowState =
      m.getOrElse(row, RowState.Empty)

    def set(loc: Loc, state: State): TableState = {
      import RowState._
      def rs = apply(loc.row)
      val rs2 = (state, loc.col) match {
        case (Some(s), Some(c)) => Cells(rs.byColumns.updated(c, s))
        case (Some(s), None   ) => WholeRow(s)
        case (None   , _      ) => Empty
      }
      new TableState(m.updated(loc.row, rs2))
    }
  }

  def emptyTableState: TableState =
    new TableState(UnivEq.emptyMap)

  type ModTable = Loc ~=> RemoteDataEditor.SetOpState

  type ModTable2 = EndoFn[TableState] ~=> Callback
}
