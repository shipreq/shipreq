package shipreq.webapp.server.lib

import net.liftweb.actor.SpecializedLiftActor
import scala.slick.jdbc.JdbcBackend.Session
import shipreq.base.db.SingleConnDatabase
import shipreq.base.util.Error
import shipreq.base.util.log.HasLogger
import shipreq.taskman.api.impl.TaskmanApi
import shipreq.taskman.api.{ApiOp, Msg, MsgId}
import shipreq.taskman.api.Msg.WebappErrorOccurred
import shipreq.webapp.base.WebappConfig
import shipreq.webapp.server.ServerConfig
import shipreq.webapp.server.app.DI
import shipreq.webapp.server.security.Oshiro
import ApiOp._

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
    WebappErrorOccurred(Oshiro.loggedInUser.map(_.id), url, s"${Error stackTraceStr e}\n\nSUPP: $suppInfo")
}

object TaskmanImpl extends TaskmanInterface with HasLogger {
  val ctx = TaskmanApi.Context(Some(ServerConfig.TaskmanSchema))

  override def run[A](op: ApiOp[A])(s: Session): A =
    new TaskmanApi(ctx, SingleConnDatabase(s)).apply(op).unsafePerformIO()

  override def runS(ops: Seq[ApiOp[_]])(s: Session): Unit = {
    val reify = new TaskmanApi(ctx, SingleConnDatabase(s))
    for (op <- ops)
      reify(op).unsafePerformIO()
  }

  override def !(m: Msg): Unit = AsyncActor ! m

  private object AsyncActor extends SpecializedLiftActor[Msg] {
    override protected def messageHandler = {
      case m: Msg =>
        try
          DI.DaoProvider.vend.withRawSession(submitMsg(m))
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
  def run[A](op: ApiOp[A])(s: Session): A
  final def runN(ops: ApiOp[_]*)(s: Session): Unit = runS(ops)(s)
  def runS(ops: Seq[ApiOp[_]])(s: Session): Unit = ops.foreach(op => run(op)(s))

  def submitMsg(m: Msg)(s: Session): MsgId       = run(SubmitMsg(m))(s)
  def submitMsgs(ms: Seq[Msg])(s: Session): Unit = run(SubmitMsgs(ms))(s)

  /** Submit msg asynchronously. */
  def !(m: Msg): Unit
}
