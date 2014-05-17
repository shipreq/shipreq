package shipreq.taskman.server.akka

import akka.actor.{Props, Actor, ActorRef}
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import shipreq.base.util.jodatime.JodaTimeHelpers.PeriodConv
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.Priority
import shipreq.taskman.server.{TaskmanLogging, TaskmanCtx, MsgHeader}

// =====================================================================================================================

object SourceActor {
  def props(ctx: TaskmanCtx) = Props(classOf[SourceActor], ctx)

  case class RequestForWork(queueStatus: Option[(Priority, Int)])
  case class IncomingWork(work: Seq[MsgHeader])
}

class SourceActor(ctx: TaskmanCtx) extends Actor with HasLogger {
  import SourceActor._
  import shipreq.taskman.server.Source
  import ctx._
  import ctx.props.taskman.{trustPeriod => _, _}

  val mdc = TaskmanLogging.mdc("source")
  val source = new Source(pollGap, queueSize)
  var state = source.empty.unsafePerformIO()

  override def receive = mdc.pf {
    case RequestForWork(qs) =>
      val (s2, ms) = source.poll(qs).run(state).unsafePerformIO()
      state = s2
      if (ms.nonEmpty)
        sender() ! IncomingWork(ms)
  }
}

// =====================================================================================================================

object ManagerActor {
  def props(ctx: TaskmanCtx, source: ActorRef) = Props(classOf[ManagerActor], ctx, source)

  case object PollSource
  case object RegisterWorker
  case object WorkAvailable
  case object RequestForWork
}

class ManagerActor(ctx: TaskmanCtx, source: ActorRef) extends Actor with HasLogger {
  import ManagerActor._
  import shipreq.taskman.server.{Manager => M}
  import context.dispatcher

  val mdc = TaskmanLogging.mdc("manager")
  val poller = context.system.scheduler.schedule(0 millis, ctx.props.taskman.pollEvery.toScala, self, PollSource)

  var workers: Set[ActorRef] = Set.empty
  var queue = M.empty

  override def postStop() = poller.cancel()

  override def receive = mdc.pf {

    case PollSource =>
      source ! SourceActor.RequestForWork(queue.status)

    case RegisterWorker =>
      workers += sender()

    case SourceActor.IncomingWork(work) =>
      log.debug z s"Received ${work.size} new msg(s)"
      queue = M.add(work).exec(queue)
      workers foreach (_ ! WorkAvailable)

    case RequestForWork =>
      val (q2, wo) = M.pop.run(queue)
      wo foreach (sender() ! _)
      queue = q2
  }
}

// =====================================================================================================================

object WorkerActor {
  import shipreq.taskman.server.WorkerId

  def props(ctx: TaskmanCtx, manager: ActorRef) = Props(classOf[WorkerActor], ctx, manager)

  private[this] val idCounter = new AtomicInteger
  def nextId() = WorkerId(idCounter.incrementAndGet().toShort)
}

class WorkerActor(ctx: TaskmanCtx, manager: ActorRef) extends Actor with HasLogger {
  import shipreq.taskman.server.Worker
  import ManagerActor.{RequestForWork, WorkAvailable}
  import ctx._

  implicit val id = WorkerActor.nextId
  val mdc = TaskmanLogging.mdc(s"worker-${id.value}")
  val worker = new Worker(ctx.msgProcessor)

  private def requestWork(): Unit =
    manager ! RequestForWork

  override def receive = mdc.pf {

    case WorkAvailable =>
      requestWork()

    case m: MsgHeader =>
      worker.process(m).unsafePerformIO()
      requestWork()
  }
}
