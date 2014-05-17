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

  val eg4 = Manager.empty + c + a + d + b

  def haveItems(ms: MsgHeader*) = be_==(ms.toList) ^^ { (q: JobQueue) => q.q.toList }

  "JoeQueue" should {
    "prefer highest priority, then oldest" in {
      eg4 must haveItems(a,b,c,d)
    }

    "add" ! prop { (q: JobQueue, ms: List[MsgHeader]) =>
      add(ms).run(q)._1.q.toList must containAllOf(q.q.toList) and containAllOf(ms.distinct)
    }

    "queue status" in {
      eg4.status ==== Some((Priority(6), 4))
    }

    "pop" ! prop {
      (q: JobQueue) => {
        val (r,(a,b)) = (for (x <- pop; y <- pop) yield (x,y)).run(q)
        (r.q.size == (q.q.size - 2).max(0)) :| "Size" &&
        ((a.toList ++ b.toList ++ r.q.toList) == q.q.toList) :| "Reconstruction" && (
        (a,b) match {
          case (Some(j), Some(k)) => (j.priority.value >= k.priority.value) :| "Wrong order"
          case (None, Some(_))    => false :| "No-pop followed by pop??"
          case _                  => true
        })
      }
    }
  }
}
