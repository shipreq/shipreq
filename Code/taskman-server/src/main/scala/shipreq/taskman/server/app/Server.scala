package shipreq.taskman.server.app

import akka.actor.ActorSystem
import akka.routing.FromConfig
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import shipreq.base.util.FxModule._
import shipreq.base.util.log.HasLogger
import shipreq.taskman.server.akka._
import shipreq.taskman.server.{TaskmanConfig, TaskmanCtx}

/**
 * Taskman, the Server.
 * Hard-working and magnanimous.
 */
object Server extends MainTemplate {

  def main(args: Array[String]): Unit =
    try {
      withTaskmanCtx { ctx =>
        startPrometheus(ctx.config.prometheus)
        Fx(run(ctx)(s => Await.result(s.system.whenTerminated, Duration.Inf)))
      }.unsafeRun()
    } catch {
      case t: Throwable =>
        logger.error("Uncaught exception. Exiting...", t)
        System.exit(1)
    }

  def run(ctx: TaskmanCtx, testConnections: Boolean = true)(f: System => Unit): Unit = {
    if (testConnections) ctx.testConnections()
    val s = new System(ctx)
    s.manager.tell(ActorMsg.RegisterWorker, s.workers)
    logger.info("Taskman started.")
    f(s)
  }

  private final class System(ctx: TaskmanCtx) extends HasLogger {
    val system = ActorSystem("Taskman")
    val source = system.actorOf(SourceActor.props(ctx), "source")
    val manager = system.actorOf(ManagerActor.props(ctx, source), "manager")
    val workers = system.actorOf(FromConfig.props(WorkerActor.props(ctx, manager)).withDispatcher("work"), "workers")

    def shutdown(): Unit = {
      logger.info("Shutting down...")
      system.terminate()
    }
  }

  private def startPrometheus(cfg: TaskmanConfig.Prometheus): Unit =
    if (cfg.enabled) {
      import io.prometheus.client.exporter._

      if (cfg.hotspot)
        io.prometheus.client.hotspot.DefaultExports.initialize()

      new HTTPServer(9031)
    }
}

//  class Terminator(app: ActorRef) extends Actor with ActorLogging {
//    context watch app
//    override def receive = {
//      case Terminated(_) =>
//        log.info("Application supervisor has terminated, shutting down...")
//        context.system.terminate()
//    }
//  }
