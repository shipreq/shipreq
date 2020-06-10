package shipreq.webapp.client.ww

import japgolly.scalajs.react.extra.Px
import japgolly.scalajs.react.{AsyncCallback, Callback, CallbackTo}
import monocle.macros.Lenses
import scala.util.Try
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.EventOrd.Implicits._
import shipreq.webapp.base.event.{EventOrd, ProjectAndOrd, VerifiedEvent}
import shipreq.webapp.base.text.PlainText

final class WebWorkerState {
  import WebWorkerState._

  private var state: Immutable =
    Immutable.init

  val getState: CallbackTo[Immutable] =
    CallbackTo(state)

  private val getStateAsync: AsyncCallback[Immutable] =
    AsyncCallback.delay(state)

  private def modState(f: Immutable => Immutable): Callback =
    Callback {
      val old = state.pao
      state = f(state)
      val projectChanged = state.pao ne old

      if (projectChanged && state.ordPromises.nonEmpty) {
        val newOrd = state.pao.ord

        def releaseFilter(p: OrdPromise): Boolean =
          if (p.ord <= newOrd) {
            // release
            p.complete.runNow()
            false // don't keep anymore
          } else
            true // keep promise

        state = Immutable.ordPromises.modify(_.filter(releaseFilter))(state)
      }
    }

  def setProject(pao: ProjectAndOrd): Callback =
    modState(_.copy(pao = pao))

  def updateProject(ves: VerifiedEvent.NonEmptySeq): Callback = {
    assert(ves.min.ord.immediatelyFollowsLatest(state.pao.ord), s"${ves.min.ord} doesn't follow ${state.pao.ord}")
    modState(Immutable.pao.modify(_.mustApplyVerified(ves)))
  }

  val pxProject: Px[Project] =
    Px(state.pao.project).withoutReuse.autoRefresh // auto cos manual is strict

  val pxPlainText: Px[PlainText.ForProject.NoCtx] =
    pxProject.map(PlainText.ForProject.noCtx.apply)

  val acProject   = pxProject  .toCallback.asAsyncCallback
  val acPlainText = pxPlainText.toCallback.asAsyncCallback

  def await(ord: Option[EventOrd.Latest]): AsyncCallback[Unit] =
    getStateAsync.flatMap { s =>
      if (ord <= s.pao.ord)
        // No need to wait
        AsyncCallback.unit
      else
        AsyncCallback.promise[Unit].asAsyncCallback.flatMap { case (promise, tryComplete) =>
          val ordPromise = OrdPromise(ord, tryComplete(tryUnit))
          val save = modState(Immutable.ordPromises.modify(ordPromise :: _)).asAsyncCallback
          save >> promise
        }
    }
}

// =====================================================================================================================

object WebWorkerState {

  @Lenses
  final case class Immutable(pao: ProjectAndOrd, ordPromises: List[OrdPromise])

  object Immutable {
    def init: Immutable =
      apply(ProjectAndOrd.empty, Nil)
  }

  final case class OrdPromise(ord: Option[EventOrd.Latest], complete: Callback)

  private[WebWorkerState] val tryUnit = Try(()) // TODO Remove after https://github.com/japgolly/scalajs-react/issues/730
}
