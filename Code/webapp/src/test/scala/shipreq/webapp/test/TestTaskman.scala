package shipreq.webapp.test

import scala.slick.session.Session
import scalaz.~>
import shipreq.webapp.app.DI
import shipreq.webapp.lib.{TaskmanImpl, TaskmanInterface}
import shipreq.taskman.api.{MsgId, Msg, ApiOp}
import ApiOp._
import scalaz.effect.IO

object TestTaskman {
  def install[R](f: => R): (R, TestTaskman) = {
    val tt = new TestTaskman
    val r = DI.Taskman.doWith(tt)(f)
    (r, tt)
  }
}

class TestTaskman extends TaskmanInterface {

  def ctx = TaskmanImpl.ctx

  val reify: (ApiOp ~> IO) = new (ApiOp ~> IO) {
    def apply[A](c: ApiOp[A]): IO[A] = c match {
      case SubmitMsg(t)   => IO{ tasksSubmitted ::= t; null.asInstanceOf[MsgId] }
      case SubmitMsgs(ts) => IO{ tasksSubmitted :::= ts.toList }
      case CfgPut(k, v)   => IO()
    }
  }

  override def run[A](s: Session, op: ApiOp[A]): A =
    synchronized(reify(op).unsafePerformIO())

  @volatile var ran: List[ApiOp[_]] = List.empty
  @volatile var tasksSubmitted: List[Msg] = List.empty
}
