package shipreq.base.test

import japgolly.microlibs.stdlib_ext.StdlibExt._
import java.time.Instant
import scala.annotation.tailrec
import scala.scalajs.js.timers.{SetIntervalHandle, SetTimeoutHandle}
import scala.util.Try
import shipreq.base.util.JsTimers

final class JsTestTimers extends JsTimers {
  import JsTestTimers._

  private var queue = List.empty[Submission]
  private var prevId = 0

  private def submit(mkId: Int => Id, interval: Double, body: => Unit): Id = {
    prevId += 1
    val id = mkId(prevId)
    val s = Submission(id, Instant.now(), interval, () => body)
    queue ::= s
    id
  }

  private def clear(id: Id): Unit =
    queue = queue.filter(_.id != id)

  override def setTimeout(interval: Double)(body: => Unit): SetTimeoutHandle =
    submit(TimeoutId, interval, body).asInstanceOf[SetTimeoutHandle]

  override def clearTimeout(handle: SetTimeoutHandle): Unit =
    handle.asInstanceOf[Any] match {
      case id: TimeoutId => clear(id)
      case _             =>
    }

  override def setInterval(interval: Double)(body: => Unit): SetIntervalHandle =
    submit(IntervalId, interval, body).asInstanceOf[SetIntervalHandle]

  override def clearInterval(handle: SetIntervalHandle): Unit =
    handle.asInstanceOf[Any] match {
      case id: IntervalId => clear(id)
      case _              =>
    }

  private def runSubmission(task: Submission, resubmitIntervals: Boolean): Try[Unit] = {
    queue = queue.filter(_ ne task)
    if (resubmitIntervals)
      task.id match {
        case _: TimeoutId => ()
        case _: IntervalId => queue ::= task.copy(submittedAt = Instant.now())
      }
    Try(task.body())
  }

  private def runNext(resubmitIntervals: Boolean): Option[Try[Unit]] =
    Option.unless(queue.isEmpty) {
      val task = queue.minBy(_.runAt)
      runSubmission(task, resubmitIntervals)
    }

  def runNext(): Option[Try[Unit]] =
    runNext(resubmitIntervals = true)

  def runAll(): List[Try[Unit]] = {
    val q = queue
    q.map(runSubmission(_, resubmitIntervals = true))
  }

  def drain(): List[Try[Unit]] = {
    @tailrec def go(result: List[Try[Unit]]): List[Try[Unit]] =
      runNext(resubmitIntervals = false) match {
        case Some(r) => go(result :+ r)
        case None    => result
      }
    go(runAll())
  }

  def isEmpty = queue.isEmpty
  @inline def nonEmpty = !isEmpty
}

object JsTestTimers {

  def apply(): JsTestTimers =
    new JsTestTimers

  private final case class Submission(id: Id, submittedAt: Instant, intervalMs: Double, body: () => Unit) {
    val runAt: Instant =
      submittedAt.plusMillis(intervalMs.toLong)
  }

  private sealed trait Id
  private final case class TimeoutId(value: Int) extends Id
  private final case class IntervalId(value: Int) extends Id
}
