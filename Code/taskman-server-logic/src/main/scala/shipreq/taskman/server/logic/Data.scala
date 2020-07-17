package shipreq.taskman.server.logic

import java.time.{Duration, Instant}
import shipreq.base.util.ArticulateError
import shipreq.taskman.api.{Priority, Task, TaskId}

final case class NodeId(value: Int) extends AnyVal

final case class WorkerId(value: Short) extends AnyVal

// =====================================================================================================================

final case class TaskHeader(id: TaskId, priority: Priority, created: Instant) {
  // override def toString = s"MsgHeader($id,$p,new DateTime(${created.getMillis}))\n"
  override def equals(other: Any): Boolean = other match {
    case TaskHeader(id2, _, _) if id.value == id2.value => true
    case _ => false
  }
  override def hashCode: Int = (id.value ^ (id.value >>> 32)).toInt
}

object TaskHeader {
  implicit def univEq: UnivEq[TaskHeader] = UnivEq.derive
}

// =====================================================================================================================

final case class TaskDetail(hdr: TaskHeader, task: Task, failureCount: Int) {
  assert(failureCount >= 0, s"Failure count = $failureCount")

  @inline def id = hdr.id
  @inline def priority = hdr.priority

  override lazy val toString =
    s"TaskDetail($hdr, ${task.toString.replace("\n", "\\n")}, $failureCount)"
}

object TaskDetail {
  implicit def univEq: UnivEq[TaskDetail] = UnivEq.derive
}

// =====================================================================================================================

final case class AssignmentTrustPeriod(value: Duration) extends AnyVal

/** Indication that an error is deliberate.
  * Deliberate errors may occur during testing and diagnosis.
  *
  * Support will not be notified when deliberate errors occur.
  */
case object Deliberate extends ArticulateError.Tag
