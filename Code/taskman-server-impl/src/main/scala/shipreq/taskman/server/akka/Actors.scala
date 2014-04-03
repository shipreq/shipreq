package shipreq.taskman.server.akka

import akka.actor.{Props, ActorLogging, Actor, ActorRef}
import java.util.concurrent.atomic.AtomicInteger
import org.joda.time.DateTime
import scala.concurrent.duration._
import scalaz.Heap
import shipreq.taskman.api.{MsgId, Priority}
import shipreq.taskman.server.{Worker, TaskmanCtx, WorkerId, MsgHeader}
import shipreq.taskman.server.Sop._

object SourceActor {
  def props(ctx: TaskmanCtx) = Props(classOf[SourceActor], ctx)

  case class RequestForWork(queueStatus: Option[(Priority, Int)])
  case class IncomingWork(work: Seq[MsgHeader])
}

class SourceActor(ctx: TaskmanCtx) extends Actor with ActorLogging {
  import SourceActor._
  import ctx._

  // TODO this logic should be in server-logic

  val minPollGapMs = 1000
  var lastPolled: DateTime = DateTime.now()

  def poll(queued: Option[(Priority, Int)]): Seq[MsgHeader] = {
    if (DateTime.now().isAfter(lastPolled plusMillis minPollGapMs)) {
      val r = sopReifier(GetMsgsAssignNode(nodeId, manager.queueSize, manager.trustPeriod, queued)).unsafePerformIO()
      lastPolled = DateTime.now()
      r
    } else
      Seq.empty
  }

  override def receive = {
    case RequestForWork(queued) =>
      val work = poll(queued)
      if (work.nonEmpty)
        sender() ! IncomingWork(work)
  }
}

// =====================================================================================================================

// TODO TaskmanCtx unneeded
object ManagerActor {
  def props(ctx: TaskmanCtx, source: ActorRef) = Props(classOf[ManagerActor], ctx, source)

  case object PollSource extends Serializable
  case object RegisterWorker
  case object WorkAvailable
  case object RequestForWork
}

class ManagerActor(ctx: TaskmanCtx, source: ActorRef) extends Actor with ActorLogging {
  import ManagerActor._
  import context.dispatcher

  // TODO Manager logic doesn't make things better here. Recreate it in reverse.
  //import shipreq.taskman.server.{Manager => M}
  //  val manager = M.Reified(ctx.manager.queueSize, ctx.manager.trustPeriod)
  //  var jobQueue: M.JobQueue = M.emptyQueue

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
  def props(ctx: TaskmanCtx, manager: ActorRef) = Props(classOf[WorkerActor], ctx, manager)

  private[this] val idCounter = new AtomicInteger
  def nextId(): WorkerId = WorkerId(idCounter.incrementAndGet().toShort)
}

class WorkerActor(ctx: TaskmanCtx, manager: ActorRef) extends Actor with ActorLogging {
  import ctx._
  import ManagerActor.{RequestForWork, WorkAvailable}

  implicit val id: WorkerId = WorkerActor.nextId
  val worker = Worker.Reified()

  private def requestWork(): Unit =
    manager ! RequestForWork

  override def receive = {

    case WorkAvailable =>
      requestWork()

    case m: MsgHeader =>
      worker.processL(m).unsafePerformIO()
      requestWork()
  }
}
