package shipreq.webapp.server.lib

import doobie.imports.Transactor
import net.liftweb.actor.SpecializedLiftActor
import scalaz.effect.IO
import scalaz.syntax.bind.ToBindOps
import shipreq.base.util.Error
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.ApiOp._
import shipreq.taskman.api.Msg.WebappErrorOccurred
import shipreq.taskman.api.impl.TaskmanApi
import shipreq.taskman.api.{ApiOp, Msg, MsgId}
import shipreq.webapp.base.WebappConfig
import shipreq.webapp.server.app.DI
import shipreq.webapp.server.security.Oshiro

object Taskman {
  import shipreq.taskman.api.CfgKeys.{Webapp => K}
  import shipreq.webapp.server.app.{AppSiteMap => SM}
  import SM.Implicits._

  def updateCfg: List[ApiOp[Unit]] = List(
    CfgPut(K.appName,  WebappConfig.appName),
    CfgPut(K.homeUrl,  SM.Home.absoluteUrl),
    CfgPut(K.loginUrl, SM.Login.absoluteUrl)
  )

  def errorMsg(e: Throwable, url: Option[String], suppInfo: String) =
    WebappErrorOccurred(
      Oshiro.loggedInUser().map(_.id),
      url,
      s"${Error stackTraceStr e}\n\nSUPP: $suppInfo")

  val ctx = TaskmanApi.Context(Some(DI.serverConfig.taskmanSchema))
}

final class TaskmanImpl(db: Transactor[IO]) extends TaskmanInterface with HasLogger {
  val api = new TaskmanApi(Taskman.ctx, db)

  override def run[A](op: ApiOp[A]): IO[A] =
    api(op)

  override def submitMsgAsync(m: Msg): IO[Unit] =
    IO(AsyncActor ! m)

  private object AsyncActor extends SpecializedLiftActor[Msg] {
    override protected def messageHandler = {
      case m: Msg =>
        try
          submitMsg(m).unsafePerformIO()
        catch {
          case e: Throwable if m.isInstanceOf[WebappErrorOccurred] =>
            log.error(e, s"Error occurred trying to send $m. FUCK.")
          case e: Throwable =>
            log.error(e, s"Error occurred send $m.")
            this ! Taskman.errorMsg(e, None, s"Was trying to send: $m")
        }
    }
  }

}

trait TaskmanInterface {
  def run[A](op: ApiOp[A]): IO[A]

  def runAll(ops: Seq[ApiOp[_]]): IO[Unit] =
    if (ops.isEmpty)
      IO.ioUnit
    else
      ops.toIterator.map(run(_)).reduce(_ >> _).map(_ => ())

  def submitMsg(m: Msg): IO[MsgId] =
    run(SubmitMsg(m))

  def submitMsgs(ms: List[Msg]): IO[List[(Msg, MsgId)]] =
    run(SubmitMsgs(ms))

  /** Submit msg asynchronously. */
  def submitMsgAsync(m: Msg): IO[Unit]
}
