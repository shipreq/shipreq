package shipreq.taskman.server.akka

import akka.actor.{Props, ActorLogging, Actor, ActorRef}
import java.util.concurrent.atomic.AtomicInteger
import org.joda.time.DateTime
import scala.concurrent.duration._
import scalaz.Heap
import shipreq.taskman.api.Priority
import shipreq.taskman.server.{WorkerId, MsgId, MsgHeader}

object SourceActor {
  def props = Props[SourceActor]

  case class RequestForWork(queueStatus: Option[(Priority, Int)])
  case class IncomingWork(work: Seq[MsgHeader])
}

class SourceActor extends Actor with ActorLogging {
  import SourceActor._

  override def receive = {
    case RequestForWork(queueStatus) =>
      val work: Seq[MsgHeader] = List(MsgHeader(MsgId(3), Priority.High, new DateTime))
      if (work.nonEmpty)
        sender() ! IncomingWork(work)
  }
}

// =====================================================================================================================

object ManagerActor {
  def props(source: ActorRef) = Props(classOf[ManagerActor], source)

  case object PollSource extends Serializable
  case object RegisterWorker
  case object WorkAvailable
  case object RequestForWork
}

class ManagerActor(source: ActorRef) extends Actor with ActorLogging {

  import ManagerActor._
  import context.dispatcher

  implicit def priOrder = scalaz.Order.fromScalaOrdering(shipreq.taskman.server.Manager.PrioritisationOrder)

  var workers: Set[ActorRef] = Set.empty
  var workQueue: Heap[MsgHeader] = Heap.Empty[MsgHeader]

  val poller = context.system.scheduler.schedule(500 millis, 2000 millis, self, PollSource)
  override def postStop() = poller.cancel()

  override def receive = {

    case PollSource =>
      val qs = {
        val s = workQueue.size
        if (s == 0) None else Some((workQueue.minimum.priority, s))
      }
      // log.debug("Asking source for work: {}", qs)
      source ! SourceActor.RequestForWork(qs) // TODO cache

    case RegisterWorker =>
      workers += sender()
      log.debug("{} registered workers.", workers.size)

    case SourceActor.IncomingWork(work) =>
      //log.debug("{} msg(s) received. New queue size is {}.", work.size, workQueue.size + work.size)
      workQueue = (workQueue /: work)((q,w) => q insert w)
      workers.foreach(_ ! WorkAvailable)

    case RequestForWork =>
      workQueue.uncons match {
        case Some((w,q)) =>
          //log.debug("Sending work {} to worker, queue size is now {}.", w, q.size)
          sender() ! w
          workQueue = q
        case None =>
        //log.debug("Work request ignored.")
      }
  }
}

// =====================================================================================================================

object WorkerActor {
  def props(manager: ActorRef) = Props(classOf[WorkerActor], manager)

  private[this] val idCounter = new AtomicInteger
  def nextId(): WorkerId = WorkerId(idCounter.incrementAndGet().toShort)
}

class WorkerActor(manager: ActorRef) extends Actor with ActorLogging {

  import ManagerActor.{RequestForWork, WorkAvailable}

  val id: WorkerId = WorkerActor.nextId

  // override def preStart() = manager ! RegisterWorker

  private def requestWork(): Unit =
    manager ! RequestForWork

  override def receive = {

    case WorkAvailable =>
      //log.debug("There's work???")
      requestWork()

    case mh: MsgHeader =>
      log.debug("Received work {}", mh)
      //??? // Do work
      // and then
      requestWork()
  }
}
