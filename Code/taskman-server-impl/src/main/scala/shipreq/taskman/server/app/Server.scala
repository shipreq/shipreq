package shipreq.taskman.server.app

import akka.actor.ActorSystem
import akka.routing.FromConfig
import shipreq.base.util.Logger
import shipreq.taskman.server.akka._
import shipreq.taskman.server.TaskmanCtx
/**
 * Taskman, the Server.
 * Hard-working and magnanimous.
 */
object Server extends MainTemplate {

  def main(args: Array[String]): Unit =
    withTaskmanCtx(ctx =>
      run(ctx)(_.system.awaitTermination()))

  def run(ctx: TaskmanCtx)(f: System => Unit): Unit = {
    val s = new System(ctx)
    s.manager.tell(ManagerActor.RegisterWorker, s.workers)
    log.info("Taskman started.")
    f(s)
  }

  class System(ctx: TaskmanCtx) extends Logger {
    val system = ActorSystem("Taskman")
    val source = system.actorOf(SourceActor.props(ctx), "source")
    val manager = system.actorOf(ManagerActor.props(ctx, source), "manager")
    val workers = system.actorOf(FromConfig.props(WorkerActor.props(ctx, manager)).withDispatcher("work"), "workers")

    def shutdown(): Unit = {
      log.info("Shutting down...")
      system.shutdown()
    }
  }
}

//  class Terminator(app: ActorRef) extends Actor with ActorLogging {
//    context watch app
//    override def receive = {
//      case Terminated(_) =>
//        log.info("Application supervisor has terminated, shutting down...")
//        context.system.shutdown()
//    }
//  }

  // Send an email
  /*
val from: EmailAddr = "whatever@gmail.com".tag
val to: EmailAddr = "japgolly+test@gmail.com".tag
val io = bopImpl(SendEmail(
  Email.Envelope(from, NonEmptyList(to)),
  Email.Content("TEST from taskman", s"Hello at ${new DateTime}.")
))
val r = io.unsafePerformIO()
log.info("DONE: {}", r)
*/