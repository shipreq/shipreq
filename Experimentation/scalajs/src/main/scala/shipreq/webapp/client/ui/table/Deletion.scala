package shipreq.webapp.client.ui.table

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.ReactVDom._
import japgolly.scalajs.react.vdom.ReactVDom.all._
import ScalazReact._
import scalaz.effect.IO
import monocle._
import monocle.function.Field2.second
import monocle.std.tuple2._

sealed trait DeletionAction
case object HardDelete extends DeletionAction
case object SoftDelete extends DeletionAction
case object Restore extends DeletionAction

class DeletionManager[S, P, D](spec: TableSpec[S, D, _, P, _, _])(
  aliveL: SimpleLens[P, Boolean],
  saveIO: D => DeletionAction => IO[Unit]) {

  private type DP = (D, P)
  private val aliveL2 = second[DP, P] composeLens aliveL

  private def modAliveS(ls: DeletionAction, alive: Boolean) =
    spec.modAndSaveS(px => saveIO(px._1)(ls).map(_ => aliveL2.set(px, alive)))
  private val restoreS    = modAliveS(Restore, true)
  private val softDeleteS = modAliveS(SoftDelete, false)
  private val hardDeleteS = spec.deleteSavedS(id => saveIO(id)(HardDelete))

  def button(T: ComponentStateFocus[S], id: D, a: DeletionAction) =
    a match {
      case HardDelete => all.button(onclick ~~> T.runState(hardDeleteS(id)), "Delete Forever")
      case SoftDelete => all.button(onclick ~~> T.runState(softDeleteS(id)), "Delete")
      case Restore    => all.button(onclick ~~> T.runState(restoreS(id)), "Restore")
    }

  def buttons(T: ComponentStateFocus[S], id: D, as: DeletionAction*) =
    as.map(button(T, id, _))

  def getSaved(T: ComponentStateFocus[S], alive: Boolean): Stream[(D, P)] =
    spec.getSaved(T).filter(px => aliveL.get(px._2) == alive)

  def getSavedP(T: ComponentStateFocus[S], alive: Boolean): Stream[P] =
    getSaved(T, alive).map(_._2)
}
