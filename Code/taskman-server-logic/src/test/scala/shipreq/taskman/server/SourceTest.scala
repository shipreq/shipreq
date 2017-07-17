package shipreq.taskman.server

import java.time.Duration
import java.time.temporal.ChronoUnit
import org.specs2.ScalaCheck
import org.specs2.mutable._
import shipreq.base.util.FxModule._
import shipreq.base.test.MockOpTransformer1
import TestHelpers._
import Sop._

class SourceTest extends Specification with ScalaCheck {
  implicit val node = NodeId(8)
  val mockMsgs = List(mh_1, mh_2)
  implicit val mockSop = MockOpTransformer1[Sop, Fx, GetMsgsAssignNode, List[MsgHeader]](SopTypeTags, mockMsgs)
  implicit val clock = Fx(timeNow)
  implicit val tp = AssignmentTrustPeriod(Duration ofDays 3)
  val source = new Source(Duration ofSeconds 1, 20)

  "poll when allowed" >> {
    val (s, ms) = source.poll(None).run(timeNow.minus(1, ChronoUnit.DAYS)).unsafeRun()
    "Time should be updated"    in { s ==== timeNow }
    "Retrieves message headers" in { ms ==== mockMsgs }
  }

  "poll before pollGap expires" >> {
    val lastPolled = timeNow minusNanos 1
    val (s, ms) = source.poll(None).run(lastPolled).unsafeRun()
    "Time should not be updated"       in { s ==== lastPolled }
    "Doesn't retrieve message headers" in { ms ==== Nil }
  }
}
