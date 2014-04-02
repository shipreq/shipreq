package shipreq.taskman.server.app

import akka.actor.ActorSystem
import akka.routing.FromConfig

/**
 * Taskman, the Server.
 * Hard-working and magnanimous.
 */
object Server extends MainTemplate {

  def main(args: Array[String]): Unit =
    withTaskmanCtx { ctx =>

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

      import shipreq.taskman.server.akka._

      val system = ActorSystem("Taskman")
      val source = system.actorOf(SourceActor.props, "source")
      val manager = system.actorOf(ManagerActor.props(source), "manager")
      val workers = system.actorOf(FromConfig.props(WorkerActor.props(manager)).withDispatcher("work"), "workers")
      manager.tell(ManagerActor.RegisterWorker, workers)
      system.awaitTermination()
    }

//  class Terminator(app: ActorRef) extends Actor with ActorLogging {
//    context watch app
//    override def receive = {
//      case Terminated(_) =>
//        log.info("Application supervisor has terminated, shutting down...")
//        context.system.shutdown()
//    }
//  }
}