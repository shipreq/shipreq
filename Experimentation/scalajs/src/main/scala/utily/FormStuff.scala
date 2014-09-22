package utily

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.ReactVDom._
import japgolly.scalajs.react.vdom.ReactVDom.all._
import ScalazReact._

import scalaz.effect.IO

import monocle._
import monocle.function.Field1.first
import monocle.function.Field2.second
import monocle.std.tuple2._

import shipreq.webapp.client.ui._
import shipreq.webapp.client.ui.Implicits._
import shipreq.webapp.client.ui.Util._
import shipreq.webapp.client.ui.table._

object FormStuff {

//  def deleteS[S, P, R](getP: S => P, save: P => IO[R], store: (S, P, R) => S) =
//    StateT[IO, S, Unit](s => {
//      val p = getP(s)
//      save(p).map(r => (store(s, p, r), ()))
//    })

  sealed trait DeletionAction
  case object HardDelete extends DeletionAction
  case object SoftDelete extends DeletionAction
  case object Restore extends DeletionAction

  class DeletionThingy[S, P, DataId](spec: TableSpec[S, DataId, _, P, _, _])(
    l: SimpleLens[P, Boolean],
    saveIO: DataId => DeletionAction => IO[Unit]) {

    private type Px = (DataId, P)
    private val hardDelS = spec.deleteSavedS(id => saveIO(id)(HardDelete))
    private val softDeleteL = second[Px, P] composeLens l
    private def aliveS(ls: DeletionAction, alive: Boolean) =
      spec.modAndSaveS(px => saveIO(px._1)(ls).map(_ => softDeleteL.set(px, alive)))
    private val softDelS = aliveS(SoftDelete, false)
    private val restoreS = aliveS(Restore, true)

    def button(T: ComponentStateFocus[S], id: DataId, a: DeletionAction) =
      a match {
        case HardDelete => all.button(onclick ~~> T.runState(hardDelS(id)))("Delete Forever")
        case SoftDelete => all.button(onclick ~~> T.runState(softDelS(id)))("Delete")
        case Restore    => all.button(onclick ~~> T.runState(restoreS(id)))("Restore")
      }

    def buttons(T: ComponentStateFocus[S], id: DataId, as: DeletionAction*) =
      as.map(button(T, id, _))

    def getSaved(T: ComponentStateFocus[S], alive: Boolean): Stream[(DataId, P)] =
      spec.getSaved(T).filter(px => l.get(px._2) == alive)

    def getSavedP(T: ComponentStateFocus[S], alive: Boolean): Stream[P] =
      getSaved(T, alive).map(_._2)
  }
}
