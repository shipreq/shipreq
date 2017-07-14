package shipreq.taskman.api.impl

import doobie.imports._
import org.specs2.mutable.Specification
import shipreq.base.test.specs2.db.DatabaseTest
import shipreq.taskman.api.{EmailAddr, MsgStatus, MsgId, Msg}

class ApiOpTest extends Specification with DatabaseTest with ApiImplTestHelpers {

  "Task submission" >> {
    "Submits a task" in {
      run_(_.submitMsg(Msg.RegistrationRequested(EmailAddr("a@b.com"), "http://x")))
      Query0[Int]("select count(1) from msgq").unique.runNow() ==== 1
    }
  }

  "Query msg status" >> {
    "When msg doesn't exist" in {
      run(_.queryMsgStatus(MsgId(123456))) must beNone
    }

    "On new msg" in {
      val id = run(_.submitMsg(Msg.RegistrationRequested(EmailAddr("a@b.com"), "http://x")))
      run(_.queryMsgStatus(id)) must beSome(MsgStatus.Unassigned)
    }
  }

}