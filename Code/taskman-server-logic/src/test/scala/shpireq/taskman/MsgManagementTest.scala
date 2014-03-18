package shpireq.taskman

import org.specs2.mutable._
import org.specs2.ScalaCheck
import org.joda.time.DateTime
import org.scalacheck.{Gen, Arbitrary}
import shipreq.taskman.api.Priority
import Arbitrary._
import MsgManagement._
import org.specs2.main.{Diffs, SmartDiffs}

class MsgManagementTest extends Specification with ScalaCheck {
  val M = MsgManagement

  val tn = DateTime.now()
  val to = tn minusMinutes 10

  val a = MsgHeader(MsgId(1), Priority(6), to)
  val b = MsgHeader(MsgId(2), Priority(6), tn)
  val c = MsgHeader(MsgId(3), Priority(5), to)
  val d = MsgHeader(MsgId(4), Priority(5), tn)

  val eg4 = M.empty + c + a + d + b

  def arbMap[B, A](f: A => B)(implicit a: Arbitrary[A]): Arbitrary[B] =
    Arbitrary { a.arbitrary.map(f) }

  implicit def arbitraryMsgId = arbMap[MsgId, Long](new MsgId(_))
  implicit def arbitraryPriority = arbMap[Priority, Short](new Priority(_))
  implicit def arbitraryDateTime = arbMap[DateTime, Long](new DateTime(_))

  implicit def arbitraryMsgHeader: Arbitrary[MsgHeader] =
    Arbitrary(for {
      i <- arbitrary[MsgId]
      p <- arbitrary[Priority]
      c <- arbitrary[DateTime]
    } yield
      MsgHeader(i,p,c))

  implicit def arbitraryJobQueue = arbMap[JobQueue, List[MsgHeader]](ms => M.empty ++ ms)

  // -------------------------------------------------------------------------------------------------------------------

  "JoeQueue" should {
    "prefer highest priority, then oldest" in {
      eg4.toList ==== List(a,b,c,d)
    }

    "addToQueue" ! prop { (q: JobQueue, ms: List[MsgHeader]) =>
      addToQueue(ms).run(q)._1 must containAllOf(q.toSeq) and containAllOf(ms.distinct)
    }

    "getHighestPriority" in {
      getHighestPriority.run(eg4) ==== (eg4, Some(Priority(6)))
    }

    "popJob" ! prop { (q: JobQueue) =>
      (for (a <- popJob; b <- popJob) yield (a,b)).run(q) match {
        case (r, (None, None))       => r == q && q.isEmpty
        case (r, (Some(j), None))    => r == q - j
        case (r, (Some(j), Some(k))) => r == q - j - k && j.p.value >= k.p.value
      }
    }

  }
}
