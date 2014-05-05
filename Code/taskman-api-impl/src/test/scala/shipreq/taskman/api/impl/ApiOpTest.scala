package shipreq.taskman.api.impl

import org.specs2.mutable.Specification
import shipreq.base.test.specs2.db.DatabaseTest
import shipreq.taskman.api.Types._
import shipreq.taskman.api.{MsgStatus, MsgId, Msg}
import shipreq.taskman.api.ApiOp.{QueryMsgStatus, SubmitMsg}

class ApiOpTest extends Specification with DatabaseTest with ApiImplTestHelpers {

  "Task submission" >> {
    "Submits a task" in {
      run_(SubmitMsg(Msg.RegistrationRequested("a@b.com".tag, "http://x")))
      sql"select count(1) from msgq".as[Int].first ==== 1
    }
  }

  "Query msg status" >> {
    "When msg doesn't exist" in {
      run(QueryMsgStatus(MsgId(123456))) must beNone
    }

    "On new msg" in {
      val id = run(SubmitMsg(Msg.RegistrationRequested("a@b.com".tag, "http://x")))
      run(QueryMsgStatus(id)) must beSome(MsgStatus.Unassigned)
    }
  }

}