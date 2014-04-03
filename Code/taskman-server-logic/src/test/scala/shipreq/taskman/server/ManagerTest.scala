package shipreq.taskman.server

import org.specs2.mutable._
import org.specs2.ScalaCheck
import org.joda.time.Period
import scalaz.effect.IO
import shipreq.taskman.api.{MsgId, Priority}
import shipreq.base.test.MockOpTransformer1
import Manager._
import Sop._
import TestHelpers._

class ManagerTest extends Specification with ScalaCheck {

  val a = MsgHeader(MsgId(1), Priority(6), timePast)
  val b = MsgHeader(MsgId(2), Priority(6), timeNow)
  val c = MsgHeader(MsgId(3), Priority(5), timePast)
  val d = MsgHeader(MsgId(4), Priority(5), timeNow)

  val eg4 = emptyQueue + c + a + d + b

  def haveItems(ms: MsgHeader*) = be_==(ms.toList) ^^ { (q: JobQueue) => q.toList }

  "JoeQueue" should {
    "prefer highest priority, then oldest" in {
      eg4 must haveItems(a,b,c,d)
    }

    "addToQueue" ! prop { (q: JobQueue, ms: List[MsgHeader]) =>
      addToQueue(ms).run(q)._1.toList must containAllOf(q.toList) and containAllOf(ms.distinct)
    }

    "getQueueStatus" in {
      getQueueStatus.run(eg4) ==== (eg4, Some((Priority(6), 4)))
    }

    "popJob" ! prop {
      (q: JobQueue) => {
        val (r,(a,b)) = (for (x <- popJob; y <- popJob) yield (x,y)).run(q)
        (r.size == (q.size - 2).max(0)) :| "Size" &&
        ((a.toList ++ b.toList ++ r.toList) == q.toList) :| "Reconstruction" && (
        (a,b) match {
          case (Some(j), Some(k)) => (j.priority.value >= k.priority.value) :| "Wrong order"
          case (None, Some(_))    => false :| "No-pop followed by pop??"
          case _                  => true
        })
      }
    }
  }

  "Manager.Reified" >> {
    implicit val node = NodeId(8)

    "pollTask with empty queue" >> {
      implicit val mockSop = MockOpTransformer1[Sop, IO, GetMsgsAssignNode, Seq[MsgHeader]](Seq(d))
      val (q, r) = Reified(20, Period days 3).pollTask(emptyQueue).unsafePerformIO()

      "Queue should have new msgs" in {
        q must haveItems(d)
      }

      "Queue status be unspecified" in {
        mockSop.soleOp.queued ==== None
      }
    }

    "pollTask with populated queue" >> {
      implicit val mockSop = MockOpTransformer1[Sop, IO, GetMsgsAssignNode, Seq[MsgHeader]](Seq(c, d))
      val (q, r) = Reified(20, Period days 3).pollTask(emptyQueue + a).unsafePerformIO()

      "Queue should have new msgs" in {
        q must haveItems(a, c, d)
      }

      "Queue status should be provided" in {
        mockSop.soleOp.queued ==== Some((Priority(6), 1))
      }
    }
  }
}
