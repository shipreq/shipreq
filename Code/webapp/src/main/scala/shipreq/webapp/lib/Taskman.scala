package shipreq.webapp.lib

import scala.slick.jdbc.JdbcBackend.Session
import shipreq.base.db.SingleConnDatabase
import shipreq.taskman.api.impl.TaskmanApi
import shipreq.taskman.api.{MsgId, Msg, ApiOp}
import shipreq.webapp.app.AppConfig
import ApiOp._

object Taskman {
  import shipreq.taskman.api.CfgKeys.{Webapp => K}
  import shipreq.webapp.app.{AppSiteMap => SM}
  import SM.Implicits._

  def updateCfg: List[ApiOp[Unit]] = List(
    CfgPut(K.appName,  AppConfig.AppName),
    CfgPut(K.homeUrl,  SM.Home.absoluteUrl),
    CfgPut(K.loginUrl, SM.Login.absoluteUrl)
  )
}

object TaskmanImpl extends TaskmanInterface {
  val ctx = TaskmanApi.Context(Some(AppConfig.TaskmanSchema))

  override def run[A](op: ApiOp[A])(s: Session): A =
    new TaskmanApi(ctx, SingleConnDatabase(s)).apply(op).unsafePerformIO()

  override def runS(ops: Seq[ApiOp[_]])(s: Session): Unit = {
    val reify = new TaskmanApi(ctx, SingleConnDatabase(s))
    for (op <- ops)
      reify(op).unsafePerformIO()
  }
}

trait TaskmanInterface {
  def run[A](op: ApiOp[A])(s: Session): A
  final def runN(ops: ApiOp[_]*)(s: Session): Unit = runS(ops)(s)
  def runS(ops: Seq[ApiOp[_]])(s: Session): Unit = ops.foreach(op => run(op)(s))

  def submitMsg(m: Msg)(s: Session): MsgId       = run(SubmitMsg(m))(s)
  def submitMsgs(ms: Seq[Msg])(s: Session): Unit = run(SubmitMsgs(ms))(s)
}
