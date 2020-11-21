package shipreq.webapp.member.project.library

import japgolly.scalajs.react.extra.Px
import japgolly.scalajs.react.{AsyncCallback, Callback, CallbackTo}
import shipreq.webapp.base.lib.LoggerJs
import shipreq.webapp.member.project.data.Project
import shipreq.webapp.member.project.event.{EventOrd, VerifiedEvent}
import shipreq.webapp.member.project.util.DataReusability.reusabilityProject

final class MutableProjectLibrary[PL <: ProjectLibrary](initialState: PL) {
  import MutableProjectLibrary.OrdPromise

  private var _state: PL =
    initialState

  val get: CallbackTo[PL] =
    CallbackTo(_state)

  private val _pxProject: Px.ThunkM[Project] =
    Px.apply(_state.latest).withReuse.manualRefresh

  val pxProject: Px[Project] =
    _pxProject

  def update(ves: VerifiedEvent.Seq): Callback =
    Callback.unless(ves.isEmpty) {
      updateBy(_.update(ves))
    }

  def update(p: Project): Callback =
    updateBy(_.update(p))

  private def updateBy(f: PL => Option[ProjectLibrary.UpdateFor[PL#This]]): Callback =
    get.flatMap { s1 =>
      Callback.traverseOption(f(s1))(u => Callback {

        // Update state
        _state = u.newLibrary.asInstanceOf[PL] // cbf jumping through hoops for type-level proof of this

        // Refresh Pxs
        val projectChanged = u.newlyAppliedEvents.nonEmpty
        if (projectChanged)
          _pxProject.refresh()

        // Complete ord promises
        if (projectChanged && _ordPromises.nonEmpty) {
          val newOrd = _state.latest.history.ordAsInt

          // Remove releasable promises
          val (releasable, pending) = _ordPromises.partition(_.ord.value <= newOrd)
          _ordPromises = pending

          // Execute releasable promises
          for (p <- releasable)
            p.complete.attempt.runNow() match {
              case Right(_) =>
              case Left(e)  => LoggerJs.exception(e)
            }
        }

      })
    }

  private var _ordPromises: List[OrdPromise] =
    Nil

  def projectAt(ord: EventOrd): AsyncCallback[Project] =
    AsyncCallback.byName {
      if (ord <= _state.ord)
        AsyncCallback.delay(_state.projectAt(ord).get)
      else
        AsyncCallback.barrier.asAsyncCallback.flatMap { barrier =>
          val ordPromise = OrdPromise(ord, barrier.complete)
          val save = AsyncCallback.delay(_ordPromises ::= ordPromise)
          save >> barrier.waitForCompletion >> projectAt(ord)
        }
    }
}

object MutableProjectLibrary {

  def apply[PL <: ProjectLibrary](initialState: PL): MutableProjectLibrary[PL] =
    new MutableProjectLibrary(initialState)

  private final case class OrdPromise(ord: EventOrd, complete: Callback)
}
