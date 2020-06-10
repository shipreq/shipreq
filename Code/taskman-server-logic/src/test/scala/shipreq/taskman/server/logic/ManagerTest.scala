package shipreq.taskman.server.logic

import org.scalacheck.Prop.propBoolean
import shipreq.base.test.MTestScalaCheck
import shipreq.taskman.api.{Priority, TaskId}
import shipreq.taskman.server.logic.Manager._
import shipreq.taskman.server.logic.TestHelpers._
import utest._

object ManagerTest extends TestSuite with MTestScalaCheck {

  private val a = TaskHeader(TaskId(1), Priority(6), timePast)
  private val b = TaskHeader(TaskId(2), Priority(6), timeNow)
  private val c = TaskHeader(TaskId(3), Priority(5), timePast)
  private val d = TaskHeader(TaskId(4), Priority(5), timeNow)

  private val eg4 = Manager.empty + c + a + d + b

  override def tests = Tests {

    "JoeQueue" - {
      "prefer highest priority, then oldest" - {
        eg4.q.toList ==> List(a, b, c, d)
      }

      "add" - scalaCheck(_.forAll { (q: JobQueue, ms: List[TaskHeader]) =>
        val r = add(ms).run(q)._1.q.toList
        (q.q.toList ::: ms.distinct).forall(r.contains)
      })

      "queue status" - {
        eg4.status ==> Some((Priority(6), 4))
      }

      "pop" - scalaCheck(_.forAll { (q: JobQueue) =>
        val (r,(a,b)) = (for (x <- pop; y <- pop) yield (x,y)).run(q)
        (r.q.size == (q.q.size - 2).max(0)) :| "Size" &&
        ((a.toList ++ b.toList ++ r.q.toList) == q.q.toList) :| "Reconstruction" && (
        (a,b) match {
          case (Some(j), Some(k)) => (j.priority.value >= k.priority.value) :| "Wrong order"
          case (None, Some(_))    => false :| "No-pop followed by pop??"
          case _                  => true
        })
      })

    }
  }
}
