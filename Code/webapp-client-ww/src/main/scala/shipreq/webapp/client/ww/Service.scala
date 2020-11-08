package shipreq.webapp.client.ww

import japgolly.scalajs.react.AsyncCallback
import shipreq.webapp.base.lib.LoggerJs
import shipreq.webapp.client.ww.graph.GraphViz.DOT
import shipreq.webapp.client.ww.api.WebWorkerCmd
import shipreq.webapp.client.ww.graph.{ProjectImpGraph, ReqImpGraph, UseCaseFlowGraph}

final class Service(logger: LoggerJs) extends Server.Service[WebWorkerCmd] {
  import WebWorkerCmd._

  val state = new WebWorkerState(logger)
  import state.Implicits._

  override def apply[A](cmd: WebWorkerCmd[A]): AsyncCallback[A] =
    cmd match {

      case Init(p, am) =>
        (state.setProject(p) >> state.setAssetManifest(am)).asAsyncCallback.ret(NoResult)

      case UpdateProject(ves) =>
        state.updateProject(ves).asAsyncCallback.ret(NoResult)

      case GraphUseCaseFlow(ord, id, ctx) =>
        state.withGraphViz(
          for {
            _ <- state.await(ord)
            p <- state.acProject
            x <- new UseCaseFlowGraph(id, p, ctx).svg
          } yield x
        )

      case GraphReqImplications(ord, focus, filterDead, colours) =>
        state.withGraphViz(
          for {
            _ <- state.await(ord)
            p <- state.acProject
            x <- new ReqImpGraph(focus, filterDead, p, colours).svg
          } yield x
        )

      case GraphAllImplications(ord, filterDead, scope, config) =>
        state.withGraphViz(
          for {
            _  <- state.await(ord)
            p  <- state.acProject
            pt <- state.acPlainText
            x  <- new ProjectImpGraph(p, pt, filterDead, scope, config).svg
          } yield x
        )

      case GraphInline(dot) =>
        state.withGraphViz(graphviz.render(DOT(dot)))
    }
}
