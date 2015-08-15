package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react.extra.{Reusability, ~=>}
import shipreq.base.util.UnivEq
import shipreq.webapp.client.app.ui.RemoteDataEditor

object Cell {

  type State = RemoteDataEditor.OpState

  type R = Row.Id
  type C = Column
  type RowState = Map[C, RemoteDataEditor.State]

  case class Loc(row: R, col: C)

  implicit val locReusability: Reusability[Loc] = Reusability.caseClass

  final class TableState(m: Map[R, RowState]) {
    def apply(id: R): RowState =
      m.getOrElse(id, UnivEq.emptyMap)

    def apply(row: R, col: C): State =
      m.get(row).flatMap(_ get col)

    def apply(loc: Loc): State =
      apply(loc.row, loc.col)

    def set(row: R, col: C)(s: State): TableState = {
      val r1 = apply(row)
      val r2 = s.fold(r1 - col)(r1.updated(col, _))
      new TableState(m.updated(row, r2))
    }

    def set(loc: Loc, s: State): TableState =
      set(loc.row, loc.col)(s)
  }

  def emptyTableState: TableState =
    new TableState(UnivEq.emptyMap)

  type ModTable = Loc ~=> RemoteDataEditor.SetOpState
}
