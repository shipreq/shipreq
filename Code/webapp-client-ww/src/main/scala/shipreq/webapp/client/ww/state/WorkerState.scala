package shipreq.webapp.client.ww.state

import japgolly.scalajs.react.extra.Px
import japgolly.scalajs.react.{AsyncCallback, Callback}
import shipreq.webapp.base.config.AssetManifest
import shipreq.webapp.base.util.AsyncVar
import shipreq.webapp.client.ww.graph.GraphViz
import shipreq.webapp.member.project.data.Project
import shipreq.webapp.member.project.event.{EventOrd, VerifiedEvent}
import shipreq.webapp.member.project.library.MutableProjectLibrary
import shipreq.webapp.member.project.text.PlainText

final class WorkerState() {

  private val assetManifest  = AsyncVar[AssetManifest]()
  private val graphViz       = AsyncVar[GraphViz]()
  private val projectLibrary = MutableProjectLibrary.empty()

  def setAssetManifest(am: AssetManifest): Callback =
    assetManifest.set(am) >> graphViz.set(GraphViz.load(am))

  def withGraphViz[A](f: GraphViz => AsyncCallback[A], retries: Int = 4): AsyncCallback[A] = {
    val main = graphViz.get.flatMap(f).attempt.timeoutMs(2000)

    def go(retries: Int): AsyncCallback[A] =
      main.flatMap {
        case Some(Right(a)) =>
          AsyncCallback.pure(a)

        case result =>
          val createNewInstance: AsyncCallback[Unit] =
            graphViz.set(GraphViz.newInstance).asAsyncCallback

          val next: AsyncCallback[A] =
            if (retries > 0)
              go(retries - 1)
            else
              result match {
                case Some(Left(err)) => AsyncCallback.throwException(err)
                case _               => AsyncCallback.throwException(new RuntimeException("Timeout rendering graph"))
              }

          createNewInstance >> next
      }

    go(retries)
  }

  def update(p: Project): Callback =
    projectLibrary.update(p)

  def update(ves: VerifiedEvent.Seq): Callback =
    projectLibrary.update(ves)

  val pxProject: Px[Project] =
    projectLibrary.pxProject

  val pxPlainText: Px[PlainText.ForProject.NoCtx] =
    pxProject.map(PlainText.ForProject.noCtx.apply)

  val acProject   = pxProject  .toCallback.asAsyncCallback
  val acPlainText = pxPlainText.toCallback.asAsyncCallback

  def await(ord: Option[EventOrd.Latest]): AsyncCallback[Unit] =
    ord match {
      case Some(o) => projectLibrary.projectAt(o).void
      case None    => AsyncCallback.unit
    }

  // For tests
  private[state] def pendingPromiseCount(): Int =
    projectLibrary.pendingPromiseCount()
}
