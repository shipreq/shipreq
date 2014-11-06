package shipreq.webapp.client.util.ui.table

import japgolly.scalajs.react._, vdom.ReactVDom._, all._, ScalazReact._
import scalaz.Need
import scalaz.effect.IO
import scalaz.syntax.equal._
import shipreq.webapp.base.data.Alive
import shipreq.webapp.base.protocol.DeletionAction, DeletionAction._
import shipreq.webapp.client.protocol.FailureIO
import shipreq.webapp.client.util.ui.Implicits._
import TableSpecD.DelIO

object TableSpecD {

  type DelIO[Arb, D] = (Arb, D, DeletionAction, FailureIO) => IO[Unit]

  def apply[Arb, S, P, D](spec: TableSpecU[Arb, S, D, _, P, _, _])
                         (aliveG: P => Alive,
                          delIO: (Arb, D, DeletionAction, FailureIO) => IO[Unit]) =
    new TableSpecD(spec, aliveG, delIO)
}

class TableSpecD[Arb, S, P, D](spec: TableSpecU[Arb, S, D, _, P, _, _], aliveG: P => Alive, delIO: DelIO[Arb, D]) {

  final protected type DP = (D, P)
  final type CSF = ComponentStateFocus[S]

  final protected def btnLabel(d: DeletionAction): String = d match {
    case Restore => "Restore"
    case SoftDel => "Delete"
    case HardDel => "Delete Forever"
  }

  @inline final def savedFilter(alive: Alive) =
    (r: SavedRowDP[D, P]) => aliveG(r.p) ≟ alive

  @inline final def savedGet(T: CSF, alive: Alive): Stream[SavedRowDP[D, P]] =
    spec savedGet T filter savedFilter(alive)

  @inline final def savedGetP(T: CSF, alive: Alive): Stream[P] =
    savedGet(T, alive).map(_.p)

  private def actionIO(T: CSF, id: D, a: DeletionAction)(implicit x: Arb): ReactST[IO, S, Unit] = {
    lazy val del: ReactST[IO, S, Unit] = {
      val rs = spec.savedStatusSetS(id)
      val f  = TableSpec.failureIO(T, rs, Need(del))
      val io = delIO(x, id, a, f)
      ReactS.retM[IO, S, Unit](io) >> rs(RowStatus.Locked).liftIO
    }
    del
  }

  def button(T: CSF, id: D, a: DeletionAction)(implicit x: Arb) =
    all.button(onclick ~~> T.runState(actionIO(T, id, a)), btnLabel(a))

  def buttons(T: CSF, id: D, as: DeletionAction*)(implicit x: Arb) =
    as.map(button(T, id, _))
}
