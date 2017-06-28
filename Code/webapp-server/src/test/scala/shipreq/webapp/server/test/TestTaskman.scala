package shipreq.webapp.server.test

import scalaz.effect.IO
import scalaz.~>
import shipreq.base.test.BaseTestUtil._
import shipreq.taskman.api.ApiOp._
import shipreq.taskman.api.{ApiOp, Msg, MsgId}
import shipreq.webapp.server.app.Global
import shipreq.webapp.server.lib.TaskmanInterface

final class TestTaskman extends TaskmanInterface {

  val reify: (ApiOp ~> IO) = new (ApiOp ~> IO) {
    def apply[A](c: ApiOp[A]): IO[A] = synchronized {
      ran ::= c
      c match {
        case SubmitMsg(m)       => IO{ msgsSubmitted ::= m; null.asInstanceOf[MsgId] }
        case SubmitMsgs(ms)     => IO{ msgsSubmitted :::= ms.toList; null.asInstanceOf[List[(Msg, MsgId)]] }
        case CfgPut(k, v)       => IO.ioUnit
        case QueryMsgStatus(id) => IO(None)
      }
    }
  }

  override def run[A](op: ApiOp[A]): IO[A] =
    synchronized(reify(op))

  @volatile var ran: List[ApiOp[_]] = List.empty
  @volatile var msgsSubmitted: List[Msg] = List.empty

  override def submitMsgAsync(m: Msg): IO[Unit] =
    submitMsg(m).map(_ => ())
}

// ---------------------------------------------------------------------------------------------------------------------

object TestTaskman {
  def use[R](run: => R): (R, TestTaskman) = {
    val old = Global.taskman
    try {
      val tt = new TestTaskman
      Global.modify(_.copy(taskman = tt))
      val r = run
      (r, tt)
    } finally
      Global.modify(_.copy(taskman = old))
  }

  // ===================================================================================================================
  // Expectations

  trait Exp {
    def test: TestTaskman => Unit
  }

  object NoTasksSubmitted extends Exp {
    override def test =
      t => if (t.msgsSubmitted.nonEmpty) fail(s"Expected zero tasks submitted, got ${t.msgsSubmitted.mkString(", ")}.")
  }

  type TaskTestPF = PartialFunction[Msg, () => Unit]

  final case class SubmittedOneTask(m: TaskTestPF) extends Exp {
    override def test = tt => tt.msgsSubmitted match {
      case t :: Nil => if (m isDefinedAt t) m(t)() else fail(s"Task didn't meet criteria: $t")
      case other    => fail(s"Expected one task submitted, got: ${other.mkString(", ")}.")
    }
  }

  private def absUrl(frag: String): String => (() => Unit) =
    url => () =>
      if (!(url.startsWith("http") && url.contains(frag)))
        fail(s"Url $url must start with http and contain '$frag'.")

  def RegistrationRequested(token: String): TaskTestPF =
    { case Msg.RegistrationRequested(_, url) => absUrl(token)(url) }

  val ReRegistrationAttempted: TaskTestPF =
    { case Msg.ReRegistrationAttempted(_) => () => () }

  val RegistrationCompleted: TaskTestPF =
    { case Msg.RegistrationCompleted(_) => () => () }

  val PasswordResetRequested: TaskTestPF =
    { case Msg.PasswordResetRequested(_, url) => absUrl("/resetpw/")(url) }
}
