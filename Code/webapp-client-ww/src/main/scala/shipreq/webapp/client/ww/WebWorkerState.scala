package shipreq.webapp.client.ww

import japgolly.scalajs.react.extra.Px
import japgolly.scalajs.react.{AsyncCallback, Callback, CallbackTo}
import monocle.macros.Lenses
import shipreq.webapp.base.AssetManifest
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.event.EventOrd.Implicits._
import shipreq.webapp.base.event.{EventOrd, ProjectAndOrd, VerifiedEvent}
import shipreq.webapp.base.text.PlainText

final class WebWorkerState {
  import WebWorkerState._

  private var _am: AssetManifest =
    null

  private var _graphviz: GraphViz =
    null

  def setAssetManifest(am: AssetManifest): Callback =
    Callback {
      this._am = am
      this._graphviz = GraphViz.load(am)
    } >> graphvizBarrier.complete

  object Implicits {

    implicit def assetManifest: AssetManifest = {
      assert(_am ne null, "WebWorkerState AssetManifest not set.")
      _am
    }

    implicit def graphviz: GraphViz = {
      assert(_graphviz ne null, "WebWorkerState GraphViz not set.")
      _graphviz
    }
  }

  private val graphvizBarrier =
    AsyncCallback.barrier.runNow()

  val awaitGraphViz: AsyncCallback[Unit] =
    graphvizBarrier.waitForCompletion

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
        AsyncCallback.barrier.asAsyncCallback.flatMap { barrier =>
          val ordPromise = OrdPromise(ord, barrier.complete)
          val save = modState(Immutable.ordPromises.modify(ordPromise :: _)).asAsyncCallback
          save >> barrier.waitForCompletion
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
}
