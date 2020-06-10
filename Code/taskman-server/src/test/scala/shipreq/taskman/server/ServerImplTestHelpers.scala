package shipreq.taskman.server

import japgolly.clearconfig._
import shipreq.base.test.db._
import shipreq.base.util.FxModule._
import shipreq.base.util.Props
import shipreq.taskman.api.TaskmanApi
import shipreq.taskman.server.logic.ServerOp

final case class ServerImplTestHelpers(xa: ImperativeXA, ctx: TaskmanCtx) {

  lazy val taskmanApi = ctx.taskmanApi
  import ctx._

  def reify[A](op: ServerOp[A]): Fx[A] =
    serverOpFx(op)

  def runApi[A](f: TaskmanApi[Fx] => Fx[A]): A =
    f(taskmanApi).unsafeRun()

  def run[A](op: ServerOp[A]): A =
    reify(op).unsafeRun()
}

object ServerImplTestHelpers {

  private[server] def cfgSrc = Props.sources

  lazy val (taskmanConfig, taskmanConfigReport) =
    TaskmanConfig.config.withReport.run(cfgSrc).unsafeRun().getOrDie()

  private lazy val emailTokenSource: ConfigSources[Fx] =
    ConfigSource.manual[Fx]("in-code")(
      "shipreq.webapp.appName" -> "ShitWreck",
      "shipreq.webapp.url.login" -> "https://shitwreck.io/login",
    )

  def use[A](f: ServerImplTestHelpers => A): A =
    TestDb.withImperativeXA { xa =>
      TaskmanCtx(xa.dbAccessor, taskmanConfig, xa, emailTokenSource).use { ctx =>
        Fx {
          val h = ServerImplTestHelpers(xa, ctx)
          f(h)
        }
      }.unsafeRun()
    }
}