package shipreq.webapp.lib

import scala.slick.session.Session
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

  override def run[A](s: Session, op: ApiOp[A]): A =
    new TaskmanApi(ctx, SingleConnDatabase(s)).apply(op).unsafePerformIO()

  override def runAll(s: Session, ops: ApiOp[_]*): Unit = {
    val reify = new TaskmanApi(ctx, SingleConnDatabase(s))
    for (op <- ops)
      reify(op).unsafePerformIO()
  }
}

trait TaskmanInterface {
  def run[A](s: Session, op: ApiOp[A]): A
  def runAll(s: Session, ops: ApiOp[_]*): Unit = ops.foreach(op => run(s, op))

  def submitMsg(m: Msg, s: Session): MsgId       = run(s, SubmitMsg(m))
  def submitMsgs(ms: Seq[Msg], s: Session): Unit = run(s, SubmitMsgs(ms))
}
