/*package sample.hello

import akka.actor.Actor
import akka.actor.Props
import akka.event.Logging
import akka.actor.ActorLogging

object Greeter {
  case object Greet
  case object Done
}

class Greeter extends Actor with ActorLogging {
//  val log = Logging(context.system, this)

  override def preStart() = {
    log.debug("preStart !!")
  }

  def receive = {
    case Greeter.Greet =>
      println("Hello World!")
log.warning("sending back!!")
      sender() ! Greeter.Done
  }
}

class HelloWorld extends Actor {
  override def preStart(): Unit = {
    val greeter = context.actorOf(Props[Greeter], "greeter")
    greeter ! Greeter.Greet
  }

  def receive = {
    // when the greeter is done, stop this actor and with it the application
    case Greeter.Done => context.stop(self)
  }
}

// ---------------------------------------------------------------------------------------------------------------------

class WorkProducer extends Actor with ActorLogging {

  import context.dispatcher
  import scala.concurrent.duration._

  case object Tick
  val ticker = context.system.scheduler.schedule(500 millis, 500 millis, self, Tick)

//  val worker = context.actorOf(Props[Worker])
  val manager = context.actorOf(Props[Workers].withDispatcher("dispatcher-X"), "manager")
  var count = 0

  override def postStop() = ticker.cancel()

  override def preStart() = {
  }

  override def receive = {
    case Tick =>
      count += 1
      log.info("Creating work: {}", count)
//      worker ! count
      manager ! count
  }
}

class Workers extends Actor with ActorLogging {
  import akka.actor.Terminated
  import akka.routing.ActorRefRoutee
  import akka.routing.Router
  import akka.routing._

  private def newWorker = {
    val r = context.actorOf(Props[Worker])
    context watch r
    r
  }

  var router = {
    val routees = Vector.fill(10)(ActorRefRoutee(newWorker))
    Router(SmallestMailboxRoutingLogic(), routees)
  }

  def receive = {
    case w: Int =>
      log.info("Routing work: {}", w)
      router.route(w, sender())
    case Terminated(a) =>
      router = router.removeRoutee(a).addRoutee(newWorker)
  }
}

class Worker extends Actor with ActorLogging {
  override def receive = {
    case x =>
      log.info("Starting work: {}", x)
      Thread.sleep(5000)
      log.info("Finished work: {}", x)
  }
}

// ---------------------------------------------------------------------------------------------------------------------

object Main {
  import akka.actor.ActorSystem
  import akka.actor.ExtendedActorSystem
  import akka.actor.Actor
  import akka.actor.Terminated
  import akka.actor.ActorLogging
  import akka.actor.Props
  import akka.actor.ActorRef
  import scala.util.control.NonFatal

  def main(args: Array[String]): Unit = {
    //akka.Main.main(Array(classOf[WorkProducer].getName))

    val system = ActorSystem("Main")

    val producer = system.actorOf(Props[WorkProducer], "producer")
    system.actorOf(Props(classOf[Terminator], producer), "terminator")

//    try {
//      val appClass = system.asInstanceOf[ExtendedActorSystem].dynamicAccess.getClassFor[Actor](args(0)).get
//      val app = system.actorOf(Props(appClass), "app")
//      val terminator = system.actorOf(Props(classOf[Terminator], app), "app-terminator")
//    } catch {
//      case NonFatal(e) ⇒ system.shutdown(); throw e
//    }
  }

  class Terminator(app: ActorRef) extends Actor with ActorLogging {
    context watch app
    def receive = {
      case Terminated(_) ⇒
        log.info("application supervisor has terminated, shutting down")
        context.system.shutdown()
    }
  }
}
*/

/*
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.ActorRef
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.Terminated

object Main2 {

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("Hello")
    val a = system.actorOf(Props[HelloWorld], "helloWorld")
    system.actorOf(Props(classOf[Terminator], a), "terminator")
  }

  class Terminator(ref: ActorRef) extends Actor with ActorLogging {
    context watch ref
    def receive = {
      case Terminated(_) =>
        log.info("{} has terminated, shutting down system", ref.path)
        context.system.shutdown()
    }
  }
}

package sample.kernel.hello

import akka.actor.{ Actor, ActorSystem, Props }
import akka.kernel.Bootable

case object Start

class HelloActor extends Actor {
  val worldActor = context.actorOf(Props[WorldActor])

  def receive = {
    case Start => worldActor ! "Hello"
    case message: String =>
      println("Received message '%s'" format message)
  }
}

class WorldActor extends Actor {
  def receive = {
    case message: String => sender() ! (message.toUpperCase + " world!")
  }
}

class HelloKernel extends Bootable {
  val system = ActorSystem("hellokernel")

  def startup = {
    system.actorOf(Props[HelloActor]) ! Start
  }

  def shutdown = {
    system.shutdown()
  }
}
*/