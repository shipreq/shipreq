package shpireq

import shipreq.taskman.api.{Msg, Priority}
import org.joda.time.DateTime

/*
 Steps
  =====

  Manager:
    Get jobs.
    Prioritise jobs.
    Assign highest-priority to worker.

  Worker:
    Allocate self to job.
    Perform job.
    Mark as success.
    Mark as failure & retry/give-up.
 */

package object taskman {

  case class NodeId(value: Int) extends AnyVal

  case class WorkerId(value: Short) extends AnyVal

  case class MsgId(value: Long) extends AnyVal

  case class MsgHeader(id: MsgId, p: Priority, created: DateTime) {
    // override def toString = s"MsgHeader($id,$p,new DateTime(${created.getMillis}))\n"
    override def equals(other: Any): Boolean = other match {
      case MsgHeader(id2, _, _) if id.value == id2.value => true
      case _ => false
    }
    override def hashCode: Int = (id.value ^ (id.value >>> 32)).toInt
  }

  case class MsgDetail(h: MsgHeader, m: Msg, failureCount: Short) {
    assert(failureCount >= 0, s"Failure count = $failureCount")
  }

}
