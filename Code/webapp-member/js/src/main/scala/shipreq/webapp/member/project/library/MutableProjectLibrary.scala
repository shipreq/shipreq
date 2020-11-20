package shipreq.webapp.member.project.library

import japgolly.scalajs.react.extra.Px
import japgolly.scalajs.react.{Callback, CallbackTo}
import shipreq.webapp.base.util.TCB
import shipreq.webapp.member.project.data.Project
import shipreq.webapp.member.project.event.VerifiedEvent
import shipreq.webapp.member.project.util.DataReusability.reusabilityProject

final class MutableProjectLibrary(initialState: ProjectLibrary) {

  private var _state: ProjectLibrary =
    initialState

  def state(): ProjectLibrary =
    _state

  val stateCB: CallbackTo[ProjectLibrary] =
    CallbackTo(_state)

  private val _pxProject: Px.ThunkM[Project] =
    Px.apply(_state.latest).withReuse.manualRefresh

  val pxProject: Px[Project] =
    _pxProject

  private def updateState(u: ProjectLibrary.Update): Callback =
    Callback {
      // if (s2.futureEvents.nonEmpty)
      //   console.warn(s"Not all events applied: stuck at #${s2.latestEventOrd.value} pending ${s2.futureEventRange}")
      _state = u.newLibrary
      if (u.newlyAppliedEvents.nonEmpty)
        _pxProject.refresh()
    }

  def applyEventSeqCB(ves: VerifiedEvent.Seq): Callback =
    Callback.unless(ves.isEmpty)(
      stateCB.flatMap { s1 =>
        Callback.traverseOption(s1.update(ves))(updateState)
      }
    )

  def applyEventSeqSCB(ves: VerifiedEvent.Seq): TCB.Success =
    TCB.Success(applyEventSeqCB(ves))
}

object MutableProjectLibrary {

  def apply(initialState: ProjectLibrary): MutableProjectLibrary =
    new MutableProjectLibrary(initialState)

}
