package shipreq.taskman.server.app

import akka.actor.ActorSystem
import akka.routing.FromConfig
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import shipreq.base.util.log.HasLogger
import shipreq.taskman.server.akka._
import shipreq.taskman.server.TaskmanCtx

/**
 * Taskman, the Server.
 * Hard-working and magnanimous.
 */
object Server extends MainTemplate {

  def main(args: Array[String]): Unit =
    withTaskmanCtx(ctx =>
      run(ctx)(s =>
        Await.result(s.system.whenTerminated, Duration.Inf)))

  def run(ctx: TaskmanCtx, testConnections: Boolean = true)(f: System => Unit): Unit = {
    ctx.logContent()
    if (testConnections) ctx.testConnections()
    val s = new System(ctx)
    s.manager.tell(ManagerActor.RegisterWorker, s.workers)
    log.info("Taskman started.")
    f(s)
  }

  class System(ctx: TaskmanCtx) extends HasLogger {
    val system = ActorSystem("Taskman")
    val source = system.actorOf(SourceActor.props(ctx), "source")
    val manager = system.actorOf(ManagerActor.props(ctx, source), "manager")
    val workers = system.actorOf(FromConfig.props(WorkerActor.props(ctx, manager)).withDispatcher("work"), "workers")

    def shutdown(): Unit = {
      log.info("Shutting down...")
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
