package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react.ReactElement
import japgolly.scalajs.react.extra.{Reusability, ~=>}
import scalaz.effect.IO
import shipreq.base.util.UnivEq

object Cell {

  trait State {
    def render: ReactElement
  }
  object State {
    def apply(f: () => ReactElement): State =
      new State { override def render = f() }

    def const(r: ReactElement): State =
      apply(() => r)

    import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._

    lazy val locked =
      Some(const(<.div("LOCKED!"))) // English

    def retry(retryFn: => IO[Unit], resumeFn: => IO[Unit]) =
      const(
        <.div(
          "Network error occurred.",
          <.button("Retry", ^.onClick ~~> retryFn), // English
          <.button("OK", ^.onClick ~~> resumeFn)) // English
      )
  }

  type R = Row.Id
  type C = Column
  type RowState = Map[C, State]

  case class Loc(row: R, col: C)
  implicit val locReusability: Reusability[Loc] = Reusability.caseclass2(Loc.unapply)

  final class TableState(m: Map[R, RowState]) {
    def apply(id: R): RowState =
      m.getOrElse(id, UnivEq.emptyMap)

    def apply(row: R, col: C): Option[State] =
      m.get(row).flatMap(_ get col)

    def apply(loc: Loc): Option[State] =
      apply(loc.row, loc.col)

    private def setState(row: R, col: C)(s: Option[State]): TableState = {
      val r1 = apply(row)
      val r2 = s.fold(r1 - col)(r1.updated(col, _))
      new TableState(m.updated(row, r2))
    }

    def set(loc: Loc, cmd: Cmd): TableState =
      setState(loc.row, loc.col)(cmd match {
        case Edit(render)     => Some(State(render))
        case Clear            => None
        case Lock             => State.locked
        case Fail(retry, ok)  => Some(State.retry(retry(), ok()))
      })
  }

  def emptyTableState: TableState =
    new TableState(UnivEq.emptyMap)

  sealed trait Cmd
  case object Clear                                           extends Cmd
  case object Lock                                            extends Cmd
  case class  Edit(render: () => ReactElement)                extends Cmd
  case class  Fail(retry: () => IO[Unit], ok: () => IO[Unit]) extends Cmd

  type ModTable = Loc ~=> ModCell
  type ModCell  = Cmd ~=> IO[Unit]

  // TODO Rename Cell.selfManage to something edit-related

  def selfManage[V](modCell     : ModCell,
                    initialValue: V)
                   (updateValue : (V, V => IO[Unit], () => Edit) => ReactElement): Edit =
    selfManageC(modCell, identity[V])(initialValue, updateValue)


  def selfManageC[V, C](modCell     : ModCell,
                        correct     : V => C)
                       (initialValue: C,
                        updateValue : (C, V => IO[Unit], () => Edit) => ReactElement): Edit = {

    lazy val updateFn: V => IO[Unit] =
      v => modCell(cmdForValue(correct(v)))

    def thisCmd: Edit =
      cmdForValue(initialValue)

    def cmdForValue(c: C): Edit =
      Edit(() => updateValue(c, updateFn, () => thisCmd))

    thisCmd
  }
}
