package shipreq.taskman.server.akka

import akka.actor.{Actor, ActorRef, Props}
import japgolly.microlibs.stdlib_ext.StdlibExt.DurationExt
import java.io.File
import java.util.concurrent.atomic.AtomicInteger
import org.apache.commons.io.FileUtils
import scala.concurrent.duration._
import shipreq.base.util.FxModule._
import shipreq.base.util.log.{HasLogger, MdcValues, TaskmanLogFields}
import shipreq.taskman.api.Priority
import shipreq.taskman.server.TaskmanCtx
import shipreq.taskman.server.logic.TaskHeader

/*
      <Time>
        |
        | (1) PollSource
        |
        v
  [------------------------------------ ManagerActor ------------------------------------]
                     |   ^                          |   ^                            |
  RequestForWork (2) |   |                          |   |             TaskHeader (6) |
                     |   | (3) IncomingWork         |   |                            |
                     v   |                          |   |                            |
      [---------- SourceActor ----------]           |   |                            |
                                                    |   |                            |
                                  WorkAvailable (4) |   | (5)                        |
                                                    |   | (7) RequestForWork         |
                                                    v   |                            v
                                           [---------------- WorkerActor ----------------]
*/

object ActorMsg {

  case object RegisterWorker

  case object PollSource

  final case class RequestForWork(queueStatus: Option[(Priority, Int)])

  final case class IncomingWork(work: Seq[TaskHeader])

  case object WorkAvailable

  case object RequestForWork
}

import ActorMsg._

private[akka] object ActorUtil {
  def mdc(actorName: String): MdcValues =
    TaskmanLogFields.work.actorName.mdc(actorName)
}

// =====================================================================================================================

object SourceActor {
  def props(ctx: TaskmanCtx) = Props(classOf[SourceActor], ctx)
}

final class SourceActor(ctx: TaskmanCtx) extends Actor with HasLogger {
  import shipreq.taskman.server.logic.Source
  import ctx._

  private val mdc    = ActorUtil.mdc("source")
  private val source = new Source(config.taskman.pollGap, config.taskman.queueSize)
  private var state  = source.empty.unsafeRun()

  private val touchHealthFile: () => Unit =
    config.taskman.healthFile.map(new File(_)) match {
      case None    => () => ()
      case Some(f) => () => FileUtils.touch(f)
    }

  override def receive = mdc.impurePF {
    case RequestForWork(qs) =>
      val (s2, tasks) = source.poll(qs).run(state).unsafeRun()
      logger.debug(s"Looking for work. Found ${tasks.size} jobs")
      state = s2
      if (tasks.nonEmpty)
        sender() ! IncomingWork(tasks)
      touchHealthFile()
  }
}

// =====================================================================================================================

object ManagerActor {
  def props(ctx: TaskmanCtx, source: ActorRef) = Props(classOf[ManagerActor], ctx, source)
}

final class ManagerActor(ctx: TaskmanCtx, source: ActorRef) extends Actor with HasLogger {
  import shipreq.taskman.server.logic.{Manager => M}
  import context.dispatcher

  private val mdc     = ActorUtil.mdc("manager")
  private val poller  = context.system.scheduler.scheduleAtFixedRate(0 millis, ctx.config.taskman.pollEvery.asFiniteDuration, self, PollSource)
  private var workers = Set.empty[ActorRef]
  private var queue   = M.empty

  override def postStop() = poller.cancel()

  override def receive = mdc.impurePF {

    case PollSource =>
      source ! RequestForWork(queue.status)

    case IncomingWork(work) =>
      queue = M.add(work).exec(queue)
      logger.info(s"Received ${work.size} new job(s). Queue size is now ${queue.size}")
      workers.foreach(_ ! WorkAvailable)

    case RequestForWork =>
      val (q2, wo) = M.pop.run(queue)
      for (job <- wo) {
        sender() ! job
        logger.debug(s"Assigned job to worker. Queue size is now ${q2.size}")
      }
      queue = q2

    case RegisterWorker =>
      workers += sender()
      logger.info(s"Registered worker. Worker count is now ${workers.size}")
  }
}

// =====================================================================================================================

object WorkerActor {
  import shipreq.taskman.server.logic.WorkerId

  def props(ctx: TaskmanCtx, manager: ActorRef) = Props(classOf[WorkerActor], ctx, manager)

  private[this] val idCounter = new AtomicInteger
  private def nextId() = WorkerId(idCounter.incrementAndGet().toShort)
}

final class WorkerActor(ctx: TaskmanCtx, manager: ActorRef) extends Actor with HasLogger {
  import shipreq.taskman.server.logic.Worker
  import ctx._

  private implicit val id = WorkerActor.nextId()
  private val mdc         = ActorUtil.mdc(s"worker-${id.value}")
  private val worker      = new Worker(ctx.businessLogic)

  private def requestWork(): Unit =
    manager ! RequestForWork

  override def receive = mdc.impurePF {

    case WorkAvailable =>
      requestWork()

    case m: TaskHeader =>
      TaskmanLogFields.work.taskId.mdc(m.id.value).impure {
        worker.process(m).unsafeRun()
      }
      requestWork()
  }
}
