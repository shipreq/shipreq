package shipreq.webapp.client.ww.state

import japgolly.scalajs.react.extra.Px
import japgolly.scalajs.react.{AsyncCallback, Callback, CallbackTo}
import monocle.macros.Lenses
import shipreq.webapp.base.config.AssetManifest
import shipreq.webapp.base.lib.LoggerJs
import shipreq.webapp.client.ww.graph.GraphViz
import shipreq.webapp.member.project.data.Project
import shipreq.webapp.member.project.event.EventOrd.Implicits._
import shipreq.webapp.member.project.event.{EventOrd, ProjectAndOrd, VerifiedEvent}
import shipreq.webapp.member.project.text.PlainText

final class WorkerState(logger: LoggerJs) {
  import WorkerState._

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
      assert(_am ne null, "WorkerState AssetManifest not set.")
      _am
    }

    implicit def graphviz: GraphViz = {
      assert(_graphviz ne null, "WorkerState GraphViz not set.")
      _graphviz
    }
  }

  private val graphvizBarrier =
    AsyncCallback.barrier.runNow()

  private val awaitGraphViz: AsyncCallback[Unit] =
    graphvizBarrier.await

  def withGraphViz[A](f: => AsyncCallback[A], retries: Int = 3): AsyncCallback[A] = {
    val main = AsyncCallback.suspend(f).attempt.timeoutMs(2000)

    def go(retries: Int): AsyncCallback[A] =
      main.flatMap {
        case Some(Right(a)) =>
          AsyncCallback.pure(a)
        case result =>
          _graphviz = GraphViz.newInstance
          if (retries > 0)
            go(retries - 1)
          else
            result match {
              case Some(Left(err)) => AsyncCallback.throwException(err)
              case _               => AsyncCallback.throwException(new RuntimeException("Timeout rendering graph"))
            }
      }

    awaitGraphViz >> go(retries)
  }

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
      logger(_.debug(s"State updated to ord=${state.pao.ord}, ordPromises=${state.ordPromises.length}"))
      val projectChanged = state.pao ne old

      if (projectChanged && state.ordPromises.nonEmpty) {
        val newOrd = state.pao.ord

        // Remove releasable promises
        val (releasable, pending) = state.ordPromises.partition(_.ord <= newOrd)
        state = state.copy(ordPromises = pending)
        logger(_.debug(s"State updated to ord=${state.pao.ord}, ordPromises=${state.ordPromises.length}"))

        // Execute releasable promises
        for (p <- releasable) {
          logger(_.debug(s"Continuing delayed request for ${p.ord}..."))
          p.complete.attempt.runNow() match {
            case Right(_)  =>
            case Left(err) => LoggerJs.exception(err)
          }
        }
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
          save >> barrier.await
        }
    }
}

// =====================================================================================================================

object WorkerState {

  @Lenses
  final case class Immutable(pao: ProjectAndOrd, ordPromises: List[OrdPromise])

  object Immutable {
    def init: Immutable =
      apply(ProjectAndOrd.empty, Nil)
  }

  final case class OrdPromise(ord: Option[EventOrd.Latest], complete: Callback)
}
