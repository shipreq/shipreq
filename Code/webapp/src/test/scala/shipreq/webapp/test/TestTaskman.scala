package shipreq.webapp.test

import shipreq.webapp.lib.{TaskmanImpl, TaskmanInterface}
import scala.slick.session.Session
import shipreq.taskman.api._
import TaskmanApi._
import shipreq.webapp.app.DI

object TestTaskman {
  def install[R](f: => R): (R, TestTaskman) = {
    val tt = new TestTaskman
    val r = DI.Taskman.doWith(tt)(f)
    (r, tt)
  }
}

class TestTaskman extends TaskmanInterface {

  def ctx = TaskmanImpl.ctx

  @inline private def run[A](cmd: Cmd[A]): Unit =
    synchronized {
      ran ::= cmd
      cmd match {
        case SubmitMsg(t) => tasksSubmitted ::= t
        case SubmitMsgs(ts) => tasksSubmitted :::= ts.toList
      }
    }

  override def submitMsg(m: Msg, s: Session) = run(SubmitMsg(m))
  override def submitMsgs(ms: Seq[Msg], s: Session) = run(SubmitMsgs(ms))

  @volatile var ran: List[Cmd[_]] = List.empty
  @volatile var tasksSubmitted: List[Msg] = List.empty
}
