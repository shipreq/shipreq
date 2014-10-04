package shipreq.webapp.client.ui.table

import monocle._
import monocle.function.Field2.second
import monocle.std.tuple2._
import scalaz.effect.IO
import scalaz.syntax.equal._
import japgolly.scalajs.react._, vdom.ReactVDom._, all._, ScalazReact._
import shipreq.webapp.shared.data.{Dead, Alive}
import shipreq.webapp.client.protocol.FailureIO

sealed abstract class DeletionAction(val btnLabel: String)
case object HardDelete extends DeletionAction("Delete Forever")
case object SoftDelete extends DeletionAction("Delete")
case object Restore    extends DeletionAction("Restore")

abstract class Deletion[S, P, D](spec: TableSpec[_, S, D, _, P, _, _], aliveL: SimpleLens[P, Alive]) {

  final protected type DP = (D, P)

  // TODO rename getSaved, here & in spec
  // TODO provide separate filter
  def getSaved(T: ComponentStateFocus[S], alive: Alive): Stream[(RowStatus, D, P)] =
    spec.getSaved(T).filter(r => aliveL.get(r._3) ≟ alive)

  def getSavedP(T: ComponentStateFocus[S], alive: Alive): Stream[P] =
    getSaved(T, alive).map(_._3)
}

// =====================================================================================================================

final class SyncDeletion[S, P, D](spec: TableSpec.SyncSave[S, D, _, P, _, _])(
    aliveL: SimpleLens[P, Alive],
    delIO: (D, DeletionAction) => IO[Unit])
    extends Deletion(spec, aliveL) {

  private val aliveL2 = second[DP, P] composeLens aliveL
  private val restoreS    = modAliveS(Restore, Alive)
  private val softDeleteS = modAliveS(SoftDelete, Dead)
  private val hardDeleteS = spec.deleteSavedS(id => delIO(id, HardDelete))
  @inline private def modAliveS(a: DeletionAction, alive: Alive) =
    spec.updateSavedSIO(dp => delIO(dp._1, a).map(_ => aliveL2.set(dp, alive)))

  def button(T: ComponentStateFocus[S], id: D, a: DeletionAction) = {
    val b = all.button(a.btnLabel)
    a match {
      case HardDelete => b(onclick ~~> T.runState(hardDeleteS(id)))
      case SoftDelete => b(onclick ~~> T.runState(softDeleteS(id)))
      case Restore    => b(onclick ~~> T.runState(restoreS(id)))
    }
  }

  def buttons(T: ComponentStateFocus[S], id: D, as: DeletionAction*) =
    as.map(button(T, id, _))
}

// =====================================================================================================================

final class AsyncDeletion[X, S, P, D](spec: TableSpec.AsyncSave[X, S, D, _, P, _, _])(
    aliveL: SimpleLens[P, Alive],
    delIO: (X, D, DeletionAction, FailureIO) => IO[Unit])
    extends Deletion(spec, aliveL) {

  def actionS(T: ComponentStateFocus[S], id: D, a: DeletionAction)(implicit x: X) = {
    val r = Some(id)
    val f = spec.failureIO(T, r)
    val j = delIO(x, id, a, f)
    ReactS.modT[IO, S](s => j.map(_ => spec.lockRow(r)(s)))
  }

  def button(T: ComponentStateFocus[S], id: D, a: DeletionAction)(implicit x: X) =
    all.button(onclick ~~> T.runState(actionS(T, id, a)), a.btnLabel)

  def buttons(T: ComponentStateFocus[S], id: D, as: DeletionAction*)(implicit x: X) =
    as.map(button(T, id, _))
}
