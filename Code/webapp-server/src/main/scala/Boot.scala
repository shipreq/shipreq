package bootstrap.liftweb

import japgolly.microlibs.config.{Config, ConfigParser, ConfigReport}
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.univeq._
import monocle.macros.Lenses
import net.liftweb.common.Logger
import net.liftweb.http._
import net.liftweb.util._
import net.liftweb.util.Props.RunModes
import scalaz.syntax.applicative._
import shipreq.base.db.{DbAccess, DbConfig}
import shipreq.base.util.FxModule._
import shipreq.base.util.{Props => ShipReqProps}
import shipreq.webapp.base.WebappConfig
import shipreq.webapp.server.ServerConfig
import shipreq.webapp.server.app._
import shipreq.webapp.server.feature.SessionStats
import shipreq.webapp.server.lib.Taskman
import shipreq.webapp.server.security.AppSecurityRealm

@Lenses
final case class AppConfig(db: DbConfig, server: ServerConfig, report: ConfigReport)

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {

  LiftRules.configureLogging()

  val packageRoot = "shipreq.webapp.server"
  lazy val logger = Logger(s"$packageRoot.Boot")

  def boot(): Unit = {

    // Read config
    val (appConfig, runMode) = readConfig()
    logger.info(appConfig.report.report)
    runMode foreach setRunMode
    logger.info(s"RunMode = ${Props.mode}")

    // Create services
    implicit val serverConfig = appConfig.server
    implicit val dbAccess = initDatabase(appConfig.db)
    initShiro()
    configureLift()
    Global.Instance = Global.default

    // Prepare services
    preloadTemplates()
    initRoutes(Global.Instance)
    initTaskman(Global.Instance)
  }

  def readConfig(): (AppConfig, Option[RunModes.Value]) = {
    import ConfigParser.Implicits.Defaults._

    val cfgRunMode: Config[Option[RunModes.Value]] =
      Config.get[String]("shipreq.lift.runMode").mapOption {
        case Some(i) => RunModes.values.iterator.filter(_.toString.toLowerCase ==* i).nextOption().map(Some(_))
        case None    => Some(None)
      }

    val plan = (DbConfig.config |@| ServerConfig.config |@| cfgRunMode).tupled.withReport
      .map { case ((a, b, r), z) => (AppConfig(a, b, z), r) }

    plan.run(ShipReqProps.sources).unsafeRun().getOrDie()
  }

  def setRunMode(runMode: RunModes.Value): Unit = {
    System.clearProperty("run.mode")
    Props.autoDetectRunModeFn.set(() => runMode)
    if (Props.mode !=* runMode)
      throw new IllegalStateException(s"Run mode (${Props.mode}) ≠ desired run mode ($runMode)")
  }

  // Stateful - adds Lift.js etc
//  def httpStatusResponder(status: Int): () => Box[LiftResponse] = {
//    val t = Templates(status.toString :: Nil)
//    assert(t.isDefined, s"Template not found for status $status: $t")
//    () => for {
//      s <- S.session
//      i <- S.request
//      o <- s.processTemplate(t, i, i.path, status)
//    } yield o
//  }


  val httpStatusResponseHeaders: List[(String, String)] =
    "Content-Type" -> "text/html;charset=utf-8" ::
    "Cache-Control" -> "no-cache,private,no-store" ::
    "Pragma" -> "no-cache" ::
    Nil

  def httpStatusResponse(status: Int): Req => InMemoryResponse = {
    val f = s"/$status.html"
    val d = LiftRules.loadResource(f).openOrThrowException(s"Template not found: $f")
    _ => InMemoryResponse(d, httpStatusResponseHeaders, S.responseCookies, status)
  }

  def configureLift(): Unit = {

    // Collect session stats
    LiftSession.afterSessionCreate ::= SessionStats.onSessionCreation _
    LiftSession.onShutdownSession ::= SessionStats.onSessionExpiration _

    // App package path
    LiftRules.addToPackages(packageRoot)

    // Prevent "stable" func names in test, and speed up generation routine
    LiftRules.funcNameGenerator = S.generateFuncName _

    // Customise URL paths for built-in resources & AJAX requests
    LiftRules.liftContextRelativePath = WebappConfig.liftPath

    // Force requests to be UTF-8
    LiftRules.early.append(_ setCharacterEncoding "UTF-8")

    // Security policies
    LiftRules.securityRules = () => {
      import ContentSourceRestriction._
      val base = List(
        Host("https:"), // allow any https content
        Scheme("data"), // webtamp inlines small assets
        Self)
      val js =
        UnsafeEval ::   // Lift itself needs this
        UnsafeInline :: // old-school form snippets (like Login) use this
        base
      val css =
        UnsafeInline :: // For React styles
        base
      val csp = ContentSecurityPolicy(
        scriptSources  = js,
        styleSources  = css,
        imageSources   = Nil,
        defaultSources = base)
      SecurityRules(
        https               = Some(HttpsRules.secure).filterNot(_ => Props.devMode || Props.testMode),
        content             = Some(csp),
        frameRestrictions   = Some(FrameRestrictions.Deny),
        enforceInDevMode    = true,
        enforceInOtherModes = true,
        logInDevMode        = true,
        logInOtherModes     = true)
    }

    // Remove X-Lift-Version
    // (This must occur *after* securityRules)
    val supplementalHeaders = LiftRules.supplementalHeaders.default.get().filterNot(_._1 == "X-Lift-Version")
    LiftRules.supplementalHeaders.default.set(() => supplementalHeaders)

    // Custom 404
    LiftRules.uriNotFound.prepend {
//      val template = NotFoundAsTemplate(ParsePath("404" :: Nil, "html", absolute = true, endSlash = false))
      val response = httpStatusResponse(404)
      NamedPF("404") { case (req, _) => NotFoundAsResponse(response(req)) }
    }

    // Custom 500 (TODO cleanup this mess)
//    LiftRules.responseTransformers.append {
//      val on500 = httpStatusResponse(500)
//      r => if (r.toResponse.code == 500) on500(r) else r
//    }
    val on500 = httpStatusResponse(500)
    LiftRules.exceptionHandler.prepend {
      case (_, req, exception) =>
        logger.error(s"500 Error serving ${req.request.uri}", exception)
        on500(req)
//         ExceptionHandler.handleServerError(req, e)
//        val content = req.normalizeHtml(S.render(<lift:embed what="500"/>, req.request))
//        XmlResponse(content.head, 500, "text/html", req.cookies)
    }
  }

  def initShiro(): Unit =
    AppSecurityRealm.init()

  def initDatabase(dbConfig: DbConfig): DbAccess = {
    val access = DbAccess.fromCfg(dbConfig).unsafeRun()
    logger.info(s"Connecting to DB: ${access.desc}")
    access.verifyConnectivity()
    access.migrator.migrate[Fx].unsafeRun()
    access
  }

  def initTaskman(g: Global): Unit =
    if (g.config.initTaskmanOnBoot)
      Taskman.updateCfg(g)
        .retryOnException((n, t) => g.config.initTaskmanRetry(n).map(d => Fx {
          logger.warn(s"Taskman initialisation error occurred. Retrying...\n${t.getMessage}")
          Thread sleep d.toMillis
        }))
        .unsafeRun()

  def preloadTemplates(): Unit = {
    import shipreq.webapp.server.snippet._
    HomeSpa
    ProjectSpa
    PublicSpa
  }

  def initRoutes(g: Global): Unit = {
    // (Must be done after Global is ready)
    LiftRules.dispatch.append(new LiftDispatcher(g).dispatchPF)
  }
}
