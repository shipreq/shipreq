package com.beardedlogic.shipreq.taskman

import akka.actor.{ActorRef, Actor, Props, ActorLogging}
import com.beardedlogic.shipreq.taskman.CrossActorMessages.{HaveSomeWork, GimmeWork}

object CrossActorMessages {

  case object GimmeWork
  case object HaveSomeWork

}

class Source extends Actor with ActorLogging {
  override def preStart() = {    log.info("STARTED!")  }

  val rnd = new scala.util.Random

  override def receive = {
    case GimmeWork =>
      log.info("Checking for work...")
//      if (rnd.nextBoolean())
      (1 to 10).foreach(_ =>
        sender() ! HaveSomeWork
      )

  }
}

class Manager(source: ActorRef, workerProps: Props) extends Actor with ActorLogging {
  import akka.routing.{Router, FromConfig}

  import context.dispatcher
  import scala.concurrent.duration._

  def cfg = context.system.settings.config
  val pollEvery = Duration(cfg.getString("manager.poll")) match {
    case d: FiniteDuration => d
    case x => throw new RuntimeException(s"Invalid polling time, not finite: $x")
  }

  case object PollForWork
  val ticker = context.system.scheduler.schedule(0 millis, pollEvery, self, PollForWork)

  val router: ActorRef =
    context.actorOf(FromConfig.props(workerProps), "router")

  override def preStart() = {    log.info("STARTED!")  }

  override def receive = {
    case PollForWork =>
      log.info("")
      source ! GimmeWork
    case HaveSomeWork =>
      log.info("Got some work!!")
      router ! HaveSomeWork
//      source ! GimmeWork
  }
}

class Worker extends Actor with ActorLogging {
  override def preStart() = {    log.info("STARTED!")  }
  override def receive = {case x => log.info("----> {}", x); Thread.sleep(2000); log.info("<----")}
}



object Main {
  import akka.actor.ActorSystem
  import akka.actor.Terminated
  import akka.actor.ActorRef

  val DedicatedDispatcher = "dedicated"
//  val WorkDispatcher = "work-dispatcher"

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("Main")
    val workerProps = Props[Worker] //.withDispatcher(WorkDispatcher)
    val source = system.actorOf(Props[Source].withDispatcher(DedicatedDispatcher), "source")
    val manager = system.actorOf(Props(classOf[Manager], source, workerProps).withDispatcher(DedicatedDispatcher), "manager")
    // system.actorOf(Props(classOf[Terminator], producer), "terminator")
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
