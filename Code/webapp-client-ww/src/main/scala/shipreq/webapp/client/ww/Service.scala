package shipreq.webapp.client.ww

import japgolly.scalajs.react.AsyncCallback
import shipreq.webapp.client.ww.api.WebWorkerCmd

object Service extends Server.Service[WebWorkerCmd] {
  import WebWorkerCmd._

  val state = new WebWorkerState

  override def apply[A](cmd: WebWorkerCmd[A]): AsyncCallback[A] =
    cmd match {

      case SetProject(p) =>
        state.setProject(p).asAsyncCallback.ret(NoResult)

      case UpdateProject(ves) =>
        state.updateProject(ves).asAsyncCallback.ret(NoResult)

      case GraphUseCaseStepFlow(ord, id, ctx) =>
        for {
          _ <- state.await(ord)
          p <- state.acProject
          x <- Graphs.useCaseStepFlow(id, p, ctx).toSvg
        } yield x

      case GraphReqImplications(ord, focus, filterDead) =>
        for {
          _ <- state.await(ord)
          p <- state.acProject
          x <- Graphs.implicationFocused(focus, filterDead, p).toSvg
        } yield x

      case GraphAllImplications(ord, filterDead, scope, config) =>
        for {
          _  <- state.await(ord)
          p  <- state.acProject
          pt <- state.acPlainText
          x  <- Graphs.implicationAll(p, pt, filterDead, scope, config).toSvg
        } yield x

    }
}
