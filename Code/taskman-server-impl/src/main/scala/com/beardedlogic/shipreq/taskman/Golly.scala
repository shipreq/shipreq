package com.beardedlogic.shipreq.taskman

//import akka.actor.{ActorRef, Actor, Props, ActorLogging}
//
//object CrossActorMessages {
//  case object GimmeWork
//  case object HaveSomeWork
//}
//import CrossActorMessages._
//
//class Source extends Actor with ActorLogging {
//  override def preStart() = {    log.info("STARTED!")  }
//
//  val rnd = new scala.util.Random
//
//  override def receive = {
//    case GimmeWork =>
//      log.info("Checking for work...")
////      if (rnd.nextBoolean())
//      (1 to 100).foreach(_ =>
//        sender() ! HaveSomeWork
//      )
//  }
//}
//
//class Manager(source: ActorRef, workerProps: Props) extends Actor with ActorLogging {
//  import akka.routing.{Router, FromConfig}
//
//  import context.dispatcher
//  import scala.concurrent.duration._
//
//  def cfg = context.system.settings.config
//  val pollEvery = Duration(cfg.getString("manager.poll")) match {
//    case d: FiniteDuration => d
//    case x => throw new RuntimeException(s"Invalid polling time, not finite: $x")
//  }
//
//  case object PollForWork
//  val ticker = context.system.scheduler.schedule(0 millis, pollEvery, self, PollForWork)
//
//  val router: ActorRef =
//    context.actorOf(FromConfig.props(workerProps), "router")
//
//  override def preStart() = {    log.info("STARTED!")  }
//
//  override def receive = {
//    case PollForWork =>
//      log.info("Polling.......")
//      source ! GimmeWork
//    case HaveSomeWork =>
//      log.info("Got some work!!")
//      router ! HaveSomeWork
////      source ! GimmeWork
//  }
//}
//
//class Worker extends Actor with ActorLogging {
//  val rnd = new scala.util.Random
//  override def preStart() = {    log.info("STARTED!")  }
//  override def receive = {case x =>
//    log.info("GOT WORK ----> {}", x)
//    Thread.sleep(1000 + (rnd.nextLong() % 5000))
//    log.info("<----")
//  }
//}
//
//// =======================================================================================================================
//
//
//object Main {
//  import akka.actor.ActorSystem
//  import akka.actor.Terminated
//  import akka.actor.ActorRef
//
//  val DedicatedDispatcher = "dedicated"
////  val WorkDispatcher = "work-dispatcher"
//
//  def main(args: Array[String]): Unit = {
//    val system = ActorSystem("Main")
//    val workerProps = Props[Worker] //.withDispatcher(WorkDispatcher)
//    val source = system.actorOf(Props[Source].withDispatcher(DedicatedDispatcher), "source")
//    val manager = system.actorOf(Props(classOf[Manager], source, workerProps).withDispatcher(DedicatedDispatcher), "manager")
//    // system.actorOf(Props(classOf[Terminator], producer), "terminator")
//  }
//
//  class Terminator(app: ActorRef) extends Actor with ActorLogging {
//    context watch app
//    def receive = {
//      case Terminated(_) ⇒
//        log.info("application supervisor has terminated, shutting down")
//        context.system.shutdown()
//    }
//  }
//}



// || M --[Q.pop]--> W ||
//  class Manager1 extends Actor with ActorLogging {
//
//    // TODO Instead of   || M --[Q.pop]--> W ||
//    // TODO it should be || M --[avail]--> W, W --[gimme]--> M, M --[Q.pop]--> W ||
//
//    implicit def priOrder = shipreq.taskman.server.Manager.PrioritisationOrder
//
//    var freeWorkers: Set[ActorRef] = Set.empty
//    var workQueue: Heap[MsgHeader] = Heap.Empty[MsgHeader]
//
//    override def receive = {
//
//      case RegisterWorker =>
//      case WorkerIsFree =>
//        workQueue.uncons match {
//          case Some((w,q)) =>
//            sender() ! w
//            workQueue = q
//          case None =>
//            freeWorkers += sender()
//        }
//
//      case IncomingWorkBatch(inc) =>
//        workQueue = (workQueue /: inc)((q,w) => q insert w)
//        freeWorkers.foreach(_ ! WorkersNeeded)
//        dist()
//    }
//
//    @annotation.tailrec
//    private def dist(): Unit = {
//      if (freeWorkers.nonEmpty)
//        workQueue.uncons match {
//          case Some((w,q)) =>
//            freeWorkers.head ! w
//            workQueue = q
//            freeWorkers = freeWorkers.tail
//            dist()
//          case None =>
//        }
//    }
//  }
// || M --[avail]--> W, W --[gimme]--> M, M --[Q.pop]--> W ||
