package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react.ReactElement
import scalaz.effect.IO
import shipreq.base.util.UnivEq
import shipreq.webapp.client.util.~=>

object Cell {

  type TableState = Map[Row.Id, RowState]

  type RowState = Map[Column, State]

  val emptyTableState: TableState =
    (UnivEq.emptyMap: TableState) withDefaultValue UnivEq.emptyMap

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
