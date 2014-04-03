package shipreq.webapp.lib

import scala.slick.session.Session
import scalaz.Free.FreeC
import shipreq.webapp.app.AppConfig
import shipreq.taskman.FreeEffect._
import shipreq.taskman.api.impl.TaskmanApiImpl._
import shipreq.taskman.api.{MsgId, Msg, ApiOp}
import ApiOp._

object Taskman {
  import shipreq.taskman.api.CfgKeys.{Webapp => K}
  import shipreq.webapp.app.{AppSiteMap => SM}
  import SM.Implicits._

  def updateCfg: FreeC[ApiOp, Unit] = (
    CfgPut(K.appName,  AppConfig.AppName) >>
    CfgPut(K.homeUrl,  SM.Home.absoluteUrl) >>
    CfgPut(K.loginUrl, SM.Login.absoluteUrl)
  )
}

object TaskmanImpl extends TaskmanInterface {

  val ctx = new GlobalContext(Some(AppConfig.TaskmanSchema))

  override def run[A](ops: FreeC[ApiOp, A], s: Session): A =
    compile(ops, reify(ctx, s)).unsafePerformIO()
}

trait TaskmanInterface {

  def run[A](ops: FreeC[ApiOp, A], s: Session): A

  def submitMsg(m: Msg, s: Session): MsgId = run(SubmitMsg(m), s)
  def submitMsgs(ms: Seq[Msg], s: Session): Unit = run(SubmitMsgs(ms), s)
}
