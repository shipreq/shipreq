package shipreq.webapp.client.ww

import japgolly.scalajs.react.AsyncCallback
import shipreq.webapp.client.ww.api.WebWorkerCmd
import shipreq.webapp.client.ww.graph.GraphViz.DOT
import shipreq.webapp.client.ww.graph.{ProjectImpGraph, ReqImpGraph, UseCaseFlowGraph}
import shipreq.webapp.member.protocol.webworker._

final class Service[Client](server: Service.Server[Client], state: WorkerState) extends ManagedWebWorker.Server.Service[Client, WebWorkerCmd] {
  import WebWorkerCmd._

  locally(server) // TODO remove

  override def apply[A](client: Client, request: WebWorkerCmd[A]): AsyncCallback[A] =
    request match {

      case i: Init =>
        state.init(i).ret(NoResult)

      case UpdateProject(u) =>
        state.update(u).asAsyncCallback.ret(NoResult)

      case GraphUseCaseFlow(ord, id, ctx) =>
        state.withGraphViz { implicit g =>
          for {
            _ <- state.await(ord)
            p <- state.acProject
            x <- new UseCaseFlowGraph(id, p, ctx).svg
          } yield x
        }

      case GraphReqImplications(ord, focus, filterDead, colours) =>
        state.withGraphViz { implicit g =>
          for {
            _ <- state.await(ord)
            p <- state.acProject
            x <- new ReqImpGraph(focus, filterDead, p, colours).svg
          } yield x
        }

      case GraphAllImplications(ord, filterDead, scope, config) =>
        state.withGraphViz { implicit g =>
          for {
            _  <- state.await(ord)
            p  <- state.acProject
            pt <- state.acPlainText
            x  <- new ProjectImpGraph(p, pt, filterDead, scope, config).svg
          } yield x
        }

      case GraphInline(dot) =>
        state.withGraphViz(_.render(DOT(dot)))
    }
}

object Service {

  type Push = Unit

  type Server[Client] = ManagedWebWorker.Server[Client, Push]

  type Maker = ManagedWebWorker.Server.ServiceMaker[WebWorkerCmd, Push]

  def maker(state: WorkerState): Maker =
    new Maker {
      override def apply[Client](server: ManagedWebWorker.Server[Client, Push]) =
        new Service(server, state)
    }
}