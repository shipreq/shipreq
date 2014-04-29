package shipreq.taskman.server

import org.joda.time.Period
import org.specs2.ScalaCheck
import org.specs2.mutable._
import scalaz.effect.IO
import shipreq.base.test.MockOpTransformer1
import TestHelpers._
import Sop._

class SourceTest extends Specification with ScalaCheck {
  implicit val node = NodeId(8)
  val mockMsgs = Seq(mh_1, mh_2)
  implicit val mockSop = MockOpTransformer1[Sop, IO, GetMsgsAssignNode, Seq[MsgHeader]](SopTypeTags, mockMsgs)
  implicit val clock = IO(timeNow)
  implicit val tp = AssignmentTrustPeriod(Period days 3)
  val source = new Source(Period seconds 1, 20)

  "poll when allowed" >> {
    val (s, ms) = source.poll(None).run(timeNow minusDays 1).unsafePerformIO()
    "Time should be updated"    in { s ==== timeNow }
    "Retrieves message headers" in { ms ==== mockMsgs }
  }

  "poll before pollGap expires" >> {
    val lastPolled = timeNow minusMillis 1
    val (s, ms) = source.poll(None).run(lastPolled).unsafePerformIO()
    "Time should not be updated"       in { s ==== lastPolled }
    "Doesn't retrieve message headers" in { ms ==== Nil }
  }
}
