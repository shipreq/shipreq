package shipreq.taskman.server

import doobie.implicits._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import shipreq.taskman.api.{EmailAddr, TaskId, TaskStatus}
import shipreq.taskman.api.Task.ReRegistrationAttempted
import shipreq.taskman.server.logic._
import shipreq.taskman.server.logic.ServerOp._
import utest._

object WorkflowTest extends TestSuite {
  private val n = NodeId(123)
  private val w = WorkerId(666)
  private val defaultTask = ReRegistrationAttempted(EmailAddr("haha cool"))

  private val assignNode = GetTasksAssignNode(n, 10, 1 minutes, None)

  private def findAndStartWork(implicit helper: ServerImplTestHelpers) = {
    import helper._

    // assign node -> cant(assign node)
    val q = run(assignNode)
    q.size ==> 1
    runApi(_.getStatus(q.head.id)) ==> Some(TaskStatus.NodeAssigned)
    run(assignNode) ==> Nil

    // assign worker -> cant(assign node, assign worker)
    val assignWorker = GetTaskAssignWorker(n, w, q.head)
    val mo = run(assignWorker)
    assert(mo.isDefined)
    val m = mo.get
    run(assignNode) ==> Nil
    run(assignWorker) ==> None
    runApi(_.getStatus(m.hdr.id)) ==> Some(TaskStatus.Working)

    (m, assignWorker)
  }

  private def queryHistory(id: TaskId)(implicit helper: ServerImplTestHelpers) =
    helper.xa ! sql"select result,failure_count from msg_history where id=${id.value}".query[(String, Int)].option

  override def tests = Tests {

    "fail then pass" - ServerImplTestHelpers.use { implicit helper =>
      import helper._

      // new
      val id = runApi(_.submit(defaultTask))
      runApi(_.getStatus(id)) ==> Some(TaskStatus.Unassigned)

      // assign node -> assign worker
      val (m1, assignWorker1) = findAndStartWork
      m1.failureCount ==> 0

      // fail:retry -> cant(assign worker)
      run(UpdateTaskRetry(n, w, m1, 0 seconds))
      run(assignWorker1) ==> None
      runApi(_.getStatus(id)) ==> Some(TaskStatus.Unassigned)

      // assign node -> assign worker
      val (m2, assignWorker2) = findAndStartWork
      m2.failureCount ==> 1

      // pass -> cant(assign node, assign worker)
      run(UpdateTaskSuccess(n, w, m2))
      run(assignNode) ==> Nil
      run(assignWorker2) ==> None
      runApi(_.getStatus(id)) ==> Some(TaskStatus.Complete)

      queryHistory(id) ==> Some(("s", 1))
    }

    "fail+delay then abort" - ServerImplTestHelpers.use { implicit helper =>
      import helper._

      // new
      val id = runApi(_.submit(defaultTask))
      runApi(_.getStatus(id)) ==> Some(TaskStatus.Unassigned)

      // assign node -> assign worker
      val (m1, assignWorker1) = findAndStartWork
      m1.failureCount ==> 0

      // fail:retry -> cant(assign worker) while delay
      run(UpdateTaskRetry(n, w, m1, 1 seconds))
      run(assignWorker1) ==> None
      run(assignNode) ==> Nil
      Thread.sleep(1050)

      // assign node -> assign worker
      val (m2, assignWorker2) = findAndStartWork
      m2.failureCount ==> 1

      // pass -> cant(assign node, assign worker)
      run(UpdateTaskAbort(n, w, m2))
      run(assignNode) ==> Nil
      run(assignWorker2) ==> None
      runApi(_.getStatus(id)) ==> Some(TaskStatus.Aborted)

      queryHistory(id) ==> Some(("f", 2))
    }

  }
}
