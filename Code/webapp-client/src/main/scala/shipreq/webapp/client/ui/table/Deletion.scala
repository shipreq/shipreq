package shipreq.webapp.client.ui.table

import monocle._
import monocle.function.Field2.second
import monocle.std.tuple2._
import scalaz.{Need, Value}
import scalaz.effect.IO
import scalaz.syntax.equal._
import japgolly.scalajs.react._, vdom.ReactVDom._, all._, ScalazReact._
import shipreq.webapp.shared.data.{Dead, Alive}
import shipreq.webapp.shared.protocol.DeletionAction, DeletionAction._
import shipreq.webapp.client.protocol.FailureIO
import shipreq.webapp.client.ui.Implicits._

abstract class Deletion[S, P, D](spec: TableSpec[_, S, D, _, P, _, _], aliveG: P => Alive) {

  final protected type DP = (D, P)
  final type CSF = ComponentStateFocus[S]

  final protected def btnLabel(d: DeletionAction): String = d match {
    case Restore => "Restore"
    case SoftDel => "Delete"
    case HardDel => "Delete Forever"
  }

  // TODO rename getSaved, here & in spec
  // TODO provide separate filter
  def getSaved(T: CSF, alive: Alive): Stream[(RowStatus, D, P)] =
    spec.savedGet(T).filter(r => aliveG(r._3) ≟ alive)

  def getSavedP(T: CSF, alive: Alive): Stream[P] =
    getSaved(T, alive).map(_._3)
}

// =====================================================================================================================

final class SyncDeletion[S, P, D](spec: TableSpec.SyncSave[S, D, _, P, _, _])(
    aliveL: SimpleLens[P, Alive],
    delIO: (D, DeletionAction) => IO[Unit])
    extends Deletion(spec, aliveL.get) {

  private val aliveL2 = second[DP, P] composeLens aliveL
  private val restoreIO_ = modAliveIO_(Restore, Alive)
  private val softDelIO_ = modAliveIO_(SoftDel, Dead)
  private val hardDelIO_ = spec.savedDeleteIO_(id => delIO(id, HardDel))
  @inline private def modAliveIO_(a: DeletionAction, alive: Alive) =
    spec.updateSavedIO_(dp => delIO(dp._1, a).map(_ => aliveL2.set(dp, alive)))

  def button(T: CSF, id: D, a: DeletionAction) = {
    val b = all.button(btnLabel(a))
    a match {
      case HardDel => b(onclick ~~> T.runState(hardDelIO_(id)))
      case SoftDel => b(onclick ~~> T.runState(softDelIO_(id)))
      case Restore => b(onclick ~~> T.runState(restoreIO_(id)))
    }
  }

  def buttons(T: CSF, id: D, as: DeletionAction*) =
    as.map(button(T, id, _))
}

// =====================================================================================================================

final class AsyncDeletion[X, S, P, D](spec: TableSpec.AsyncSave[X, S, D, _, P, _, _])(
    aliveG: P => Alive,
    delIO: (X, D, DeletionAction, FailureIO) => IO[Unit])
    extends Deletion(spec, aliveG) {

  private def actionIO(T: CSF, id: D, a: DeletionAction)(implicit x: X): ReactST[IO, S, Unit] = {
    lazy val del: ReactST[IO, S, Unit] = {
      val row = Some(id)
      val f = spec.failureIO(T, row, Need(del))
      val io = delIO(x, id, a, f)
      ReactS.retM(io) >> spec.lockRowS(row).liftIO
    }
    del
  }

  def button(T: CSF, id: D, a: DeletionAction)(implicit x: X) =
    all.button(onclick ~~> T.runState(actionIO(T, id, a)), btnLabel(a))

  def buttons(T: CSF, id: D, as: DeletionAction*)(implicit x: X) =
    as.map(button(T, id, _))
}
