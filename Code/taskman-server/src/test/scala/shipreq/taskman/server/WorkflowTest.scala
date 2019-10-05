package shipreq.taskman.server

import doobie.imports._
import japgolly.microlibs.stdlib_ext.StdlibExt._
import shipreq.base.util.FxModule._
import shipreq.taskman.api.{EmailAddr, MsgId, MsgStatus}
import shipreq.taskman.api.Msg.ReRegistrationAttempted
import shipreq.taskman.server.logic._
import shipreq.taskman.server.logic.ServerOp._
import utest._

object WorkflowTest extends TestSuite {
  private val n = NodeId(123)
  private val w = WorkerId(666)
  private val defaultMsg = ReRegistrationAttempted(EmailAddr("haha cool"))

  private val assignNode = GetMsgsAssignNode(n, 10, 1 minutes, None)

  private def findAndStartWork(implicit helper: ServerImplTestHelpers) = {
    import helper._

    // assign node -> cant(assign node)
    val q = run(assignNode)
    q.size ==> 1
    runApi(_.queryMsgStatus(q.head.id)) ==> Some(MsgStatus.NodeAssigned)
    run(assignNode) ==> Nil

    // assign worker -> cant(assign node, assign worker)
    val assignWorker = GetMsgAssignWorker(n, w, q.head)
    val mo = run(assignWorker)
    assert(mo.isDefined)
    val m = mo.get
    run(assignNode) ==> Nil
    run(assignWorker) ==> None
    runApi(_.queryMsgStatus(m.hdr.id)) ==> Some(MsgStatus.Working)

    (m, assignWorker)
  }

  private def queryHistory(id: MsgId)(implicit helper: ServerImplTestHelpers) =
    sql"select result,failure_count from msg_history where id=${id.value}".query[(String, Int)]
      .option.transact(helper.xa).unsafeRun()

  override def tests = Tests {

    "fail then pass" - ServerImplTestHelpers.imperative() { implicit helper =>
      import helper._

      // new
      val id = runApi(_.submitMsg(defaultMsg))
      runApi(_.queryMsgStatus(id)) ==> Some(MsgStatus.Unassigned)

      // assign node -> assign worker
      val (m1, assignWorker1) = findAndStartWork
      m1.failureCount ==> 0

      // fail:retry -> cant(assign worker)
      run(UpdateMsgRetry(n, w, m1, 0 seconds))
      run(assignWorker1) ==> None
      runApi(_.queryMsgStatus(id)) ==> Some(MsgStatus.Unassigned)

      // assign node -> assign worker
      val (m2, assignWorker2) = findAndStartWork
      m2.failureCount ==> 1

      // pass -> cant(assign node, assign worker)
      run(UpdateMsgSuccess(n, w, m2))
      run(assignNode) ==> Nil
      run(assignWorker2) ==> None
      runApi(_.queryMsgStatus(id)) ==> Some(MsgStatus.Complete)

      queryHistory(id) ==> Some(("s", 1))
    }

    "fail+delay then abort" - ServerImplTestHelpers.imperative() { implicit helper =>
      import helper._

      // new
      val id = runApi(_.submitMsg(defaultMsg))
      runApi(_.queryMsgStatus(id)) ==> Some(MsgStatus.Unassigned)

      // assign node -> assign worker
      val (m1, assignWorker1) = findAndStartWork
      m1.failureCount ==> 0

      // fail:retry -> cant(assign worker) while delay
      run(UpdateMsgRetry(n, w, m1, 1 seconds))
      run(assignWorker1) ==> None
      run(assignNode) ==> Nil
      Thread.sleep(1050)

      // assign node -> assign worker
      val (m2, assignWorker2) = findAndStartWork
      m2.failureCount ==> 1

      // pass -> cant(assign node, assign worker)
      run(UpdateMsgAbort(n, w, m2))
      run(assignNode) ==> Nil
      run(assignWorker2) ==> None
      runApi(_.queryMsgStatus(id)) ==> Some(MsgStatus.Aborted)

      queryHistory(id) ==> Some(("f", 2))
    }

  }
}
