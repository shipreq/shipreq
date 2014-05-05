package shipreq.taskman.server

import org.specs2.mutable._
import org.specs2.ScalaCheck
import shipreq.taskman.api.{MsgId, Priority}
import Manager._
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
}
