package shipreq.taskman.api.impl

import doobie._
import shipreq.base.test.BaseTestUtil._
import shipreq.base.test.db.TestDb
import shipreq.taskman.api.{EmailAddr, Task, TaskId, TaskStatus}
import utest._

object ApiOpTest extends TestSuite {

  override def tests = Tests {
    val api = TaskmanApiImpl(TestDb.db.schema)

    "Task submission" - {
      "Submits a task" - {
        val r: Int = TestDb ! {
          for {
            _ <- api.submit(Task.RegistrationRequested(EmailAddr("a@b.com"), "http://x"))
            c <- Query0[Int]("select count(1) from msgq").unique
          } yield c
        }
        assertEq(r, 1)
      }
    }

    "Query msg status" - {

      "When msg doesn't exist" - {
        val r = TestDb ! api.getStatus(TaskId(123456))
        assertEq(r, None)
      }

      "On new msg" - {
        val r = TestDb ! (
          for {
            id <- api.submit(Task.RegistrationRequested(EmailAddr("a@b.com"), "http://x"))
            s <- api.getStatus(id)
          } yield s
        )
        assertEq(r, Some(TaskStatus.Unassigned))
      }
    }

  }
}
