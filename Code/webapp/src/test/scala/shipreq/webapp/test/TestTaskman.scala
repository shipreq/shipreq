package shipreq.webapp.test

import scala.slick.session.Session
import scalaz.~>
import scalaz.Free.FreeC
import shipreq.webapp.app.DI
import shipreq.webapp.lib.{TaskmanImpl, TaskmanInterface}
import shipreq.taskman.FreeEffect._
import shipreq.taskman.api.{MsgId, Msg, ApiOp}
import ApiOp._

object TestTaskman {
  def install[R](f: => R): (R, TestTaskman) = {
    val tt = new TestTaskman
    val r = DI.Taskman.doWith(tt)(f)
    (r, tt)
  }
}

class TestTaskman extends TaskmanInterface {

  def ctx = TaskmanImpl.ctx

  val trans: (ApiOp ~> IOM) = new (ApiOp ~> IOM) {
    def apply[A](c: ApiOp[A]): IOM[A] = c match {
      case SubmitMsg(t)   => iom{ tasksSubmitted ::= t; null.asInstanceOf[MsgId] }
      case SubmitMsgs(ts) => iom{ tasksSubmitted :::= ts.toList }
      case CfgPut(k, v)   => iom{}
    }
  }

  override def run[A](ops: FreeC[ApiOp, A], s: Session): A =
    synchronized(compile(ops, trans).unsafePerformIO())

  @volatile var ran: List[ApiOp[_]] = List.empty
  @volatile var tasksSubmitted: List[Msg] = List.empty
}
