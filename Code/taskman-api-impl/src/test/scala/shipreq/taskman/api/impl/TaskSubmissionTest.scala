package shipreq.taskman.api.impl

import org.specs2.mutable.Specification
import shipreq.base.test.db.specs2.DatabaseTest
import shipreq.taskman.api.Types._
import shipreq.taskman.api.{Msg, ApiOp}
import ApiOp.SubmitMsg
import ApiOp.Effect._
import TaskmanApiImpl._

class TaskSubmissionTest extends Specification with DatabaseTest {

  "Submits task" in {
    val cmd = SubmitMsg(Msg.RegistrationRequested("a@b.com".tag, "http://x"))
    compile(cmd, reify(new GlobalContext(None), session)).unsafePerformIO()
    sql"select count(1) from msgq".as[Int].first ==== 1
  }

}