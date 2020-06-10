package shipreq.taskman.server.app

import akka.actor.ActorSystem
import akka.routing.FromConfig
import cats.effect.{ExitCode, IO}
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
object Server extends TaskmanApp {

  override def run(args: List[String]): IO[ExitCode] =
    withTaskmanCtx { ctx =>
      val main: Fx[Unit] =
        for {
          _ <- startPrometheus(ctx.config.prometheus)
          s <- startAkka(ctx)
          _ <- Fx(Await.result(s.system.whenTerminated, Duration.Inf))
        } yield ()

      main.attempt.map {
        case Right(_) => ExitCode.Success
        case Left(e) =>
          logger.error("Uncaught exception. Exiting...", e)
          ExitCode.Error
      }
    }

  private def startPrometheus(cfg: TaskmanConfig.Prometheus): Fx[Unit] =
    Fx {
      if (cfg.enabled) {
        import io.prometheus.client.exporter._

        if (cfg.hotspot)
          io.prometheus.client.hotspot.DefaultExports.initialize()

        new HTTPServer(9031)
      }
    }

  def startAkka(ctx: TaskmanCtx): Fx[System] = Fx {
    val s = new System(ctx)
    s.manager.tell(ActorMsg.RegisterWorker, s.workers)
    logger.info("Taskman started.")
    s
  }

  final class System(ctx: TaskmanCtx) extends HasLogger {
    val system = ActorSystem("Taskman")
    val source = system.actorOf(SourceActor.props(ctx), "source")
    val manager = system.actorOf(ManagerActor.props(ctx, source), "manager")
    val workers = system.actorOf(FromConfig.props(WorkerActor.props(ctx, manager)).withDispatcher("work"), "workers")

    def shutdown(): Unit = {
      logger.info("Shutting down...")
      system.terminate()
    }
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
