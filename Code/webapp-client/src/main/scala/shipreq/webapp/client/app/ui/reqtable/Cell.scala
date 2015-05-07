package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react.ReactElement
import scalaz.effect.IO
import shipreq.base.util.UnivEq
import shipreq.webapp.client.util.~=>

object Cell {

  type RowState = Map[Column, State]

  final class TableState(m: Map[Row.Id, RowState]) {
    def apply(id: Row.Id): RowState =
      m.getOrElse(id, UnivEq.emptyMap)

    def apply(row: Row.Id, col: Column): Option[State] =
      m.get(row).flatMap(_ get col)

    def set(row: Row.Id, col: Column)(s: Option[State]): TableState = {
      val r1 = apply(row)
      val r2 = s.fold(r1 - col)(r1.updated(col, _))
      new TableState(m.updated(row, r2))
    }

    def set(cmd: SetCmd): TableState =
      set(cmd.row, cmd.col)(cmd.cellState)
  }

  def emptyTableState: TableState =
    new TableState(UnivEq.emptyMap)

  case class SetCmd(row: Row.Id, col: Column, cellState: Option[State])

  type SetIO = SetCmd ~=> IO[Unit]

  sealed trait State

  /**
   * The editor of a cell is active.
   *
   * This trait is expected to be extended to contain and manage its own state.
   */
  trait Editing extends State {
    def render: ReactElement
  }
  object Editing {
    def apply(f: => ReactElement): Editing =
      new Editing { override def render = f }
  }

  def selfManage[V](setState: Option[State] => IO[Unit], initialValue: V)
                   (updateValue: (V, V => IO[Unit]) => ReactElement): State =
    selfManageC(setState, identity[V])(initialValue, updateValue)

  def selfManageC[V, C](setState: Option[State] => IO[Unit], correct: V => C)
                       (initialValue: C, updateValue: (C, V => IO[Unit]) => ReactElement): State = {

    lazy val updateFn: V => IO[Unit] =
      v => setState(Some(stateForValue(correct(v))))

    def stateForValue(c: C): State =
      Editing(updateValue(c, updateFn))

    stateForValue(initialValue)
  }
}
