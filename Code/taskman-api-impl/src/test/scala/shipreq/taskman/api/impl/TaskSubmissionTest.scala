package shipreq.taskman.api.impl

import org.specs2.mutable.Specification
import shipreq.base.test.db.specs2.DatabaseTest
import shipreq.taskman.api.Types._
import shipreq.taskman.api.{Msg, ApiOp}
import ApiOp.SubmitMsg

class TaskSubmissionTest extends Specification with DatabaseTest with ApiImplTestHelpers {

  "Submits task" in {
    runApiOp(SubmitMsg(Msg.RegistrationRequested("a@b.com".tag, "http://x")))
    sql"select count(1) from msgq".as[Int].first ==== 1
  }

}