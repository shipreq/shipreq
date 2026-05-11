package shipreq.webapp.server.logic.test

import cats.Eval
import com.typesafe.scalalogging.StrictLogging
import shipreq.taskman.api.{Task, TaskId, TaskStatus, TaskmanApi}
import shipreq.webapp.member.test.WebappTestUtil._

final class MockTaskman extends TaskmanApi[Eval] with StrictLogging {
  private var prevMsgId = 0L
  var msgs = Vector.empty[(TaskId, Task)]

  def reset(): Unit = {
    prevMsgId = 0L
    msgs = Vector.empty
  }

  override def cfgPut(key: String, value: String) = Eval.always[Unit] {
    ()
  }

  override def submit(m: Task) = Eval.always[TaskId] {
    prevMsgId += 1
    val id = TaskId(prevMsgId)
    msgs :+= ((id, m))
    logger.debug(s"Submitted to Taskman: $id $m")
    id
  }

  override def getStatus(id: TaskId) = Eval.always[Option[TaskStatus]] {
    None
  }

  def assertSubmitted(expect: Int): Unit =
    if (msgs.length !=* expect)
      fail(s"Expected $expect Taskman tasks submitted, got ${msgs.length}: ${msgs.mkString(", ")}")

  def assertSubmits[A](expect: Int)(a: => A): A =
    assertDifference("taskman.assertSubmits", msgs.length)(expect)(a)

  def assertLastSubmitted[A](pf: PartialFunction[Task, A]): A =
    if (msgs.isEmpty)
      fail("No tasks submitted.")
    else
      pf.lift(msgs.last._2) getOrElse
        fail(s"Unexpected Taskman task submitted: ${msgs.last._2}")

//  def assertSubmitted(msg: Msg*): Unit =
//    assertEq(msg.toVector, tasksSubmitted.map(_._2))
}
