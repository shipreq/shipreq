package shipreq.taskman.api.impl

import org.specs2.mutable.Specification
import shipreq.base.test.db.specs2.DatabaseTest
import shipreq.taskman.api.Types._
import shipreq.taskman.api._
import TaskmanApi._
import Effect._
import TaskmanApiImpl.reify

class TaskSubmissionTest extends Specification with DatabaseTest {

  "Submits task" in {
    val cmd = SubmitTask1(TaskDef.RegistrationRequested("a@b.com".tag, None))
    compile(cmd, reify(session)).unsafePerformIO()
    sql"select count(1) from task".as[Int].first ==== 1
  }

}
