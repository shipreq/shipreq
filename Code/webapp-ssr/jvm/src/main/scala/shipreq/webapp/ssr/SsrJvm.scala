package shipreq.webapp.ssr

import com.typesafe.scalalogging.StrictLogging
import japgolly.scalagraal._
import shipreq.webapp.client.public.PublicSpaProtocols.{InitData => PublicInitData}
import GraalJs._
import GraalBoopickle._

// TODO Make this a minimal trait, wrap results in F[_], handle errors
final class SsrJvm(ctx: ContextSync) extends StrictLogging {

  private[this] val exprPublic =
    Expr.compileFnCall1[PublicInitData]("public")(_.asString.timed)

  // TODO Take url and userAgent too
  def public(i: PublicInitData): String = {
    val Right((time, s)) = ctx.eval(exprPublic(i))
    logger.info(s"SSR:public completed in $time")
    s
  }
}

object SsrJvm {

  // TODO Initialise in Boot, allow off via Config, store in Global
  lazy val TEMP = apply()

  def apply(): SsrJvm = {
    val init = (
      Expr("window = {console: console, location: {protocol: 'https:', hostname: 'shipreq.com', port:'', href: 'https://shipreq.com'}, navigator: {userAgent: ''}}")
      >> Expr.requireFileOnClasspath("webapp-ssr-deps.js")
      >> Expr.requireFileOnClasspath("webapp-ssr.js"))

    val ctx = ContextSync()
    ctx.eval(init).left.toOption.foreach(e => throw e.underlying)

    // TODO wait what? This would be around each eval, not each context!
    // Rename with aroundEval, add {before|after}Eval?
    //.withAround(ContextSync.Around.before(init(_).left.toOption.foreach(e => throw e.underlying)))

    new SsrJvm(ctx)
  }

  // TODO Remove SsrJvm.main
  def main(args: Array[String]): Unit = {
    val ssr = apply()

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
