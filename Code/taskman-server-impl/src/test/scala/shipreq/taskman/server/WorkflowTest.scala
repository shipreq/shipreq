package shipreq.taskman.server

import org.specs2.matcher.ThrownExpectations
import org.specs2.mutable.Specification
import org.specs2.time.NoTimeConversions
import shipreq.base.test.specs2.db.DatabaseTest
import shipreq.base.util.jodatime.JodaTimeHelpers._
import shipreq.taskman.api.{EmailAddr, MsgStatus, MsgId}
import shipreq.taskman.api.Msg.ReRegistrationAttempted
import shipreq.taskman.api.ApiOp.{QueryMsgStatus, SubmitMsg}
import shipreq.taskman.server.Sop._

class WorkflowTest extends Specification with DatabaseTest with NoTimeConversions with ThrownExpectations
    with ServerImplTestHelpers {

  override def mutex = dbMutexR

  val n = NodeId(123)
  val w = WorkerId(666)
  val defaultMsg = ReRegistrationAttempted(EmailAddr("haha cool"))

  val assignNode = GetMsgsAssignNode(n, 10, 1 minutes, None)

  def findAndStartWork = {
    // assign node -> cant(assign node)
    val q = run(assignNode)
    q must have size 1
    run(QueryMsgStatus(q.head.id)) must beSome(MsgStatus.NodeAssigned)
    run(assignNode) must beEmpty

    // assign worker -> cant(assign node, assign worker)
    val assignWorker = GetMsgAssignWorker(n, w, q.head)
    val mo = run(assignWorker)
    mo must beSome
    val m = mo.get
    run(assignNode) must beEmpty
    run(assignWorker) must beNone
    run(QueryMsgStatus(m.hdr.id)) must beSome(MsgStatus.Working)

    (m, assignWorker)
  }

  def queryHistory(id: MsgId) =
    sql"select result,failure_count from msg_history where id=${id.value}".as[(String,Int)].firstOption

  "Workflow: fail then pass" in {
    // new
    val id = run(SubmitMsg(defaultMsg))
    run(QueryMsgStatus(id)) must beSome(MsgStatus.Unassigned)

    // assign node -> assign worker
    val (m1, assignWorker1) = findAndStartWork
    m1.failureCount must_== 0

    // fail:retry -> cant(assign worker)
    run(UpdateMsgRetry(n, w, m1, 0 sec))
    run(assignWorker1) must beNone
    run(QueryMsgStatus(id)) must beSome(MsgStatus.Unassigned)

    // assign node -> assign worker
    val (m2, assignWorker2) = findAndStartWork
    m2.failureCount must_== 1

    // pass -> cant(assign node, assign worker)
    run(UpdateMsgSuccess(n, w, m2))
    run(assignNode) must beEmpty
    run(assignWorker2) must beNone
    run(QueryMsgStatus(id)) must beSome(MsgStatus.Complete)

    queryHistory(id) must_== Some(("s", 1))
  }

  "Workflow: fail+delay then abort" in {
    // new
    val id = run(SubmitMsg(defaultMsg))
    run(QueryMsgStatus(id)) must beSome(MsgStatus.Unassigned)

    // assign node -> assign worker
    val (m1, assignWorker1) = findAndStartWork
    m1.failureCount must_== 0

    // fail:retry -> cant(assign worker) while delay
    run(UpdateMsgRetry(n, w, m1, 1 sec))
    run(assignWorker1) must beNone
    run(assignNode) must beEmpty
    Thread.sleep(1050)

    // assign node -> assign worker
    val (m2, assignWorker2) = findAndStartWork
    m2.failureCount must_== 1

    // pass -> cant(assign node, assign worker)
    run(UpdateMsgAbort(n, w, m2))
    run(assignNode) must beEmpty
    run(assignWorker2) must beNone
    run(QueryMsgStatus(id)) must beSome(MsgStatus.Aborted)

    queryHistory(id) must_== Some(("f", 2))
  }
}
