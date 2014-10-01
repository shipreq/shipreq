package shipreq.webapp.client.ui.table

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.ReactVDom._
import japgolly.scalajs.react.vdom.ReactVDom.all._
import ScalazReact._
import scalaz.effect.IO
import scalaz.syntax.equal._
import monocle._
import monocle.function.Field2.second
import monocle.std.tuple2._
import shipreq.webapp.shared.data.{Dead, Alive}

sealed trait DeletionAction
case object HardDelete extends DeletionAction
case object SoftDelete extends DeletionAction
case object Restore extends DeletionAction

class DeletionManager[S, P, D](spec: TableSpec[S, D, _, P, _, _])(
  aliveL: SimpleLens[P, Alive],
  saveIO: D => DeletionAction => IO[Unit]) {

  private type DP = (D, P)
  private val aliveL2 = second[DP, P] composeLens aliveL

  private def modAliveS(ls: DeletionAction, alive: Alive) =
    spec.modAndSaveS(px => saveIO(px._1)(ls).map(_ => aliveL2.set(px, alive)))
  private val restoreS    = modAliveS(Restore, Alive)
  private val softDeleteS = modAliveS(SoftDelete, Dead)
  private val hardDeleteS = spec.deleteSavedS(id => saveIO(id)(HardDelete))

  def button(T: ComponentStateFocus[S], id: D, a: DeletionAction) =
    a match {
      case HardDelete => all.button(onclick ~~> T.runState(hardDeleteS(id)), "Delete Forever")
      case SoftDelete => all.button(onclick ~~> T.runState(softDeleteS(id)), "Delete")
      case Restore    => all.button(onclick ~~> T.runState(restoreS(id)), "Restore")
    }

  def buttons(T: ComponentStateFocus[S], id: D, as: DeletionAction*) =
    as.map(button(T, id, _))

  // TODO rename getSaved, here & in spec
  // TODO provide separate filter
  def getSaved(T: ComponentStateFocus[S], alive: Alive): Stream[(D, P)] =
    spec.getSaved(T).filter(px => aliveL.get(px._2) ≟ alive)

  def getSavedP(T: ComponentStateFocus[S], alive: Alive): Stream[P] =
    getSaved(T, alive).map(_._2)
}
