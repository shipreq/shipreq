package shipreq.webapp.ssr

import com.typesafe.scalalogging.StrictLogging
import japgolly.scalagraal._
import japgolly.scalagraal.GraalJs._
import japgolly.scalagraal.GraalBoopickle._
import shipreq.base.util.FxModule._
import SsrAlgebra.Types._

final class SsrInterpreter(ctx: ContextSync) extends SsrAlgebra[Fx] with StrictLogging {

  private[this] val exprPublic =
    Expr.compileFnCall1[PublicInitData]("public")(_.asString.timed)

  override def public(i: PublicInitData) = Fx {
    // TODO Add evalOrThrow
    ctx.eval(exprPublic(i)) match {

      case Right((time, html)) =>
        // TODO evalTimed would be better than timed on the Expr (what about a MetricsWriter?)
        logger.info("SSR:public completed in %,d ms".format(time.toMillis))
        Html(html)

      case Left(e) =>
        throw e.underlying
    }
  }
}

object SsrInterpreter {

  def apply(prometheus: Boolean): SsrInterpreter = {
    val setup = (
      Expr("window = {console: console, location: {protocol: 'https:', hostname: 'shipreq.com', port:'', href: 'https://shipreq.com'}, navigator: {userAgent: ''}}")
        >> Expr.requireFileOnClasspath("webapp-ssr-deps.js")
        >> Expr.requireFileOnClasspath("webapp-ssr.js"))

    var ctxBuilder = ContextSync.Builder.fixedContext()
      .onContextCreate(setup)

    if (prometheus) {
      val w = GraalPrometheus.Builder().registerAndBuild()
      ctxBuilder = ctxBuilder.writeMetrics(w)
    }

    val ctx = ctxBuilder
      .writeMetrics(ContextMetrics.Print())
      .build()

    new SsrInterpreter(ctx)
  }

  // TODO Remove SsrInterpreter.main
  def main(args: Array[String]): Unit = {
    val ssr = apply(false)

    import shipreq.base.util.Allow
    import shipreq.webapp.base.protocol.{ServerSideProc, ServerSideProcId}
    import shipreq.webapp.client.public.PublicSpaProtocols._

    val sspId = ServerSideProcId("")

    val landingPage = ServerSideProc(sspId, LandingPage.Fn)
    val register1 = ServerSideProc(sspId, Register.Fn1)
    val register2 = ServerSideProc(sspId, Register.Fn2)
    val login = ServerSideProc(sspId, Login.Fn)
    val resetPassword1 = ServerSideProc(sspId, ResetPassword.Fn1)
    val resetPassword2 = ServerSideProc(sspId, ResetPassword.Fn2)

    val i = PublicInitData(
      publicRegistration = Allow,
      loggedInUser = None,
      landingPage = landingPage,
      register1 = register1,
      register2 = register2,
      login = login,
      resetPassword1 = resetPassword1,
      resetPassword2 = resetPassword2)

    (0 to 10).foreach(_ => ssr.public(i))
    println(ssr.public(i))
  }
}