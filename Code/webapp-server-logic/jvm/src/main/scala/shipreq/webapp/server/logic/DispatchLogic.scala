package shipreq.webapp.server.logic

import com.typesafe.scalalogging.StrictLogging
import io.circe._
import io.circe.syntax._
import japgolly.microlibs.nonempty.NonEmptySet
import japgolly.microlibs.stdlib_ext.ParseLong
import japgolly.microlibs.utils.Utils
import japgolly.univeq._
import java.time.Instant
import scalaz.syntax.monad._
import scalaz.{-\/, Monad, \/, \/-}
import shipreq.base.ops.Trace
import shipreq.base.util._
import shipreq.webapp.base.Urls
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol._
import shipreq.webapp.base.protocol.binary.SafePickler
import shipreq.webapp.base.protocol.binary.SafePickler.DecodingFailure
import shipreq.webapp.base.user._
import shipreq.webapp.client.public.PublicSpaProtocols
import shipreq.webapp.server.ServerLogicConfig
import shipreq.webapp.server.logic.dispatch.{Request => _, _}

/** Usage:
  *
  * 1. Wire up [[DispatchLogic.Ops]] to session-less dispatch.
  *    Use [[DispatchLogic.Ops.candidate]] as the condition and [[DispatchLogic.Ops.total]] as the handler.
  *
  * 2. Wire up [[DispatchLogic.mainDispatcher()]] to normal (session-dependent) dispatch.
  */
object DispatchLogic {

  @inline private[this] def isSepChar(c: Char): Boolean =
    c == '/' || c == '#'

  /** If arg is /home this will match:
    *
    * /home
    * /home/…
    * /home#…
    */
  def spaTest(au: Url.Relative): dispatch.Request[Any] => Boolean = {
    val a = au.relativeUrlNoHeadSlash
    val aLen = a.length
    req => {
      val bu = req.path
      val b = bu.relativeUrlNoHeadSlash
      val cmp = b.length - aLen
      if (cmp == 0)
        b == a
      else
        (cmp > 0) && isSepChar(b(aLen)) && b.startsWith(a)
    }
  }

  def spaTest1(au: Url.Relative.Param1[_]): dispatch.Request[Any] => Option[String] = {
    val prefix = au.prefixNoHeadSlash
    val prefixLen = prefix.length
    req => {
      val path = req.path.relativeUrlNoHeadSlash
      Option.when(path.startsWith(prefix)) {
        var i = prefixLen
        val pathLastIdx = path.length - 1
        while (i <= pathLastIdx && !isSepChar(path(i)))
          i += 1
        path.substring(prefixLen, i)
      }
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  /** Prefix for all ops routes */
  val opsRoot = Url.Relative("/ops")

  val opsSecretKey = "secret"

  /** This is a lazy, hardcoded password to validate admin access. When ShipReq gets serious this shit will be
    * replaced with proper authentication and authorisation.
    */
  val opsSecretValue = PlainTextPassword("Hooquail2aehiey1viemiefaayengeiGhuch8Eishee3OHu4aiKieth3lieshaid")

  /** FOR UNIT-TESTS ONLY */
  val unitTestLoginUrl = Url.Relative("/c8c8f430-93b2-43fe-a072-11f9a1ab52a0")
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

final class DispatchLogic[F[_], RealReq](readRealReq: RealReq => dispatch.Request[RealReq])
                                        (implicit F: Monad[F],
                                         config    : ServerLogicConfig,
                                         db        : DB.VerificationTokenReadOnly[F],
                                         metrics   : MetricsLogic[F],
                                         ops       : OpsEndpoints[F],
                                         common    : CommonProtocolLogic[F],
                                         publicSpa : PublicSpaLogic[F],
                                         homeSpa   : HomeSpaLogic.Ajax[F],
                                         security  : Security.Algebra[F],
                                         svr       : Server.Time[F],
                                         tracer    : TraceLogic[F, RealReq, dispatch.Response]) {

  import DispatchLogic._
  import Method._

  private type Request = dispatch.Request[RealReq]

  @inline private def traceUrl(url: Url.Relative, f: F[Response])(implicit req: Request): F[Response] =
    traceUrlWithSpan(url, _ => f)

  private def traceUrlWithSpan(url: Url.Relative, f: tracer.Span => F[Response])(implicit req: Request): F[Response] =
    metrics.setHttpName(url.relativeUrl) >> tracer.http(url.relativeUrl, req.real, req.path)(f)

  private def onMethod(m: Method)(f: F[Response])(implicit req: Request): F[Response] =
    if (req.method eq m)
      f
    else
      F.pure(ResponseCmd.StatusOnly.MethodNotAllowed.withoutCookieUpdate)

  // Occasional type inference problem
  @inline private def when[A](cond: Request => Boolean)(ok: Request => F[A]): Request ?=> F[A] = FnWithFallback.when(cond)(ok)
  @inline private def extract[E, A](cond: Request => Option[E])(ok: Request => E => F[A]): Request ?=> F[A] = FnWithFallback.extract(cond)(ok)

  private def whenUrlIs[A](url: Url.Relative): (Request => F[A]) => Request ?=> F[A] =
    when(_.path ==* url)

  private def whenUrlIsAnyOf[A](urls: NonEmptySet[Url.Relative]): (Request => F[A]) => Request ?=> F[A] = {
    import Url.dropTailSlashes
    val norm: Url.Relative => String = u => dropTailSlashes(u.underlying)
    val lookup = Utils.quickStringExists(urls.whole.map(norm))
    when(r => lookup(norm(r.path)))
  }

  private def get(url: Url.Relative, resp: F[Response]): Request ?=> F[Response] =
    whenUrlIs(url)(implicit req =>
      traceUrl(req.path,
        onMethod(Get)(resp)))

  private def getF(url: Url.Relative, resp: Request => F[Response]): Request ?=> F[Response] =
    whenUrlIs(url)(implicit req =>
      traceUrl(req.path,
        onMethod(Get)(resp(req))))

  private def spa(root: Url.Relative, onGet: (tracer.Span, Request) => F[Response]): Request ?=> F[Response] =
    when(spaTest(root))(implicit req =>
      traceUrlWithSpan(root, span =>
        onMethod(Get)(onGet(span, req))))

  private def spaWithObfuscatedParam[A](url       : Url.Relative.Param1[Obfuscated[A]])
                                       (obfuscator: Obfuscator[A])
                                       (onGet     : (tracer.Span, Request, String \/ A) => F[Response]): Request ?=> F[Response] =
    extract(spaTest1(url))(implicit req => param =>
      traceUrlWithSpan(url.prefix, span =>
        onMethod(Get)(onGet(span, req, obfuscator.deobfuscate(Obfuscated(param))))))

  private def parseParams[A](parsed: Option[A])(f: A => F[Response]): F[Response] =
    parsed match {
      case Some(a) => f(a)
      case None    => F pure ResponseCmd.Text(StatusCode.BadRequest, "Invalid params.").withoutCookieUpdate
    }

  /**
    * Re-scopes routes so that they must first match a prefix.
    *
    * Eg. /a & /b can be re-scoped under /x resulting in /x/a and /x/b
    */
  private def scope[A](prefix: Url.Relative, routes: Request ?=> A): Request ?=> A = {
    val test = prefix.isEqualToOrParentOf
    val mod = prefix.removeSelfOrParent
    routes.embed(r => test(r.path), r => r.copy(path = mod(r.path)))
  }

  private def loadOrInitSession(cmd: Security.SessionToken[Any] => ResponseCmd)(implicit req: Request): F[Response] =
    for {
      s  <- security.sessionRestoreOrCreate(req.cookie)
      cu <- security.sessionPersistIfNew(s)
    } yield Response(cmd(s), cu)

  private def requireSession(fCmd: Security.SessionToken[Instant] => F[Response])(implicit req: Request): F[Response] =
    security.sessionRestore(req.cookie).flatMap {
      case Security.SessionRestoreResult.Success(t) => fCmd(t)
      case Security.SessionRestoreResult.None
         | Security.SessionRestoreResult.Expired(_) => F pure ResponseCmd.StatusOnly.Forbidden.withoutCookieUpdate
    }

  private def cmdWhenLoginRequired(implicit req: Request): ResponseCmd =
    ResponseCmd.Redirect(
      if (req.path ==* Urls.memberHome)
        Urls.login
      else
        Urls.login / req.path.relativeUrlNoHeadSlash)

  private def needAuth(f: User => F[ResponseCmd])(implicit span: tracer.Span, req: Request): F[Response] =
    for {
      session  <- security.sessionRestoreOrCreate(req.cookie)
      response <- session.authenticatedUser match {
                    case Some(u) =>
                      for {
                        _   <- tracer.alg.addAttrs(Trace.Attr.ShipReqUserId(u.id.value) :: Nil)
                        cmd <- f(u)
                        cu  <- security.sessionPersistIfNew(session)
                      } yield Response(cmd, cu)
                    case None =>
                      F pure Response(cmdWhenLoginRequired, Cookie.Update.empty)
                  }
    } yield response

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object Main {

    val publicSpa: Request ?=> F[Response] = {
      import Urls.{PublicSpaRoute => R}

      // This logic is mirrored in .public.spa.Routes
      val login: Request ?=> F[Response] =
        spa(R.Login.url, (_, req) =>
          security.sessionRestoreOrCreate(req.cookie).flatMap { s =>
            if (s.authenticatedUser.isEmpty)
              for {
                cu <- security.sessionPersistIfNew(s)
              } yield Response(ResponseCmd.ServePublicSpa(s.authenticatedUser), cu)
            else
              F pure Response(ResponseCmd.redirectToMemberHome, Cookie.Update.empty)
          })

      val staticRoutes: Request ?=> F[Response] =
        whenUrlIsAnyOf(R.static.filterNot(_ ==* R.Login).get.map(_.url).toNES) { implicit req =>
          traceUrl(req.path,
            onMethod(Get)(
              loadOrInitSession(t =>
                ResponseCmd.ServePublicSpa(t.authenticatedUser))))
        }

      val verificationTokenFn: R.NeedsToken => VerificationToken => F[VerificationToken.Status] = {
        case R.Register2     => PublicSpaLogic.tokenStatusFn(db.getUserRegistrationTokenIssueDate, config.security.registrationTokenLifespan)
        case R.ResetPassword => PublicSpaLogic.tokenStatusFn(db.getResetPasswordTokenIssueDate, config.security.passwordResetTokenLifespan)
      }

      val verificationTokenRoutes: Request ?=> F[Response] =
        R.needsToken.map { r =>
          val getTokenStatus = verificationTokenFn(r)
          extract(spaTest1(r.url)) { implicit req => param =>
            val token = VerificationToken(param)
            traceUrl(r.url.prefix,
              onMethod(Get)(
                security.protect(
                  for {
                    status <- getTokenStatus(token)
                    resp   <- loadOrInitSession(s =>
                                status match {
                                  case VerificationToken.Status.Valid   => ResponseCmd.ServePublicSpa(s.authenticatedUser)
                                  case VerificationToken.Status.Invalid => ResponseCmd.redirectToPublicHome
                                  case VerificationToken.Status.Expired => ResponseCmd.redirectToPublicHome // could be better but good enough
                                }
                              )
                  } yield resp
                )))
          }
        }.reduce(_ | _)

      login | staticRoutes | verificationTokenRoutes
    }

    val memberHomeSpa: Request ?=> F[Response] =
      spa(Urls.memberHome, needAuth(F pure ResponseCmd.ServeHomeSpa(_))(_, _))

    val projectSpa: Request ?=> F[Response] =
      spaWithObfuscatedParam(Urls.project)(Obfuscators.projectId) { (span, req, result) =>
        implicit val _span = span
        implicit val _req = req
        result match {
          case \/-(projectId) =>
            tracer.alg.addAttrs(Trace.Attr.ShipReqProjectId(projectId) :: Nil) >>
            needAuth(user =>
              security.db.getProjectOwner(projectId).map {
                case Some(o) if o ==* user.id => ResponseCmd.ProjectSpa.Serve(user, projectId)
                case Some(_)                  => ResponseCmd.ProjectSpa.NotOwner
                case None                     => ResponseCmd.ProjectSpa.InvalidId
              }
            )
          case -\/(_) => F pure Response(ResponseCmd.ProjectSpa.InvalidId, Cookie.Update.empty)
        }
      }

    val logout: Request ?=> F[Response] =
      getF(Urls.logout, req =>
        for {
          cu <- SimpleEndpoints.logout(req.cookie)
        } yield Response(ResponseCmd.redirectToPublicHome, cu)
      )

    val routes: Request ?=> F[Response] =
      publicSpa | memberHomeSpa | projectSpa | logout

    val fallback: Request => F[Response] = { implicit req =>
      val cmd =
        if (req.method eq Get)
          ResponseCmd.redirectToPublicHome
        else
          ResponseCmd.StatusOnly.MethodNotAllowed
      F.pure(Response(cmd, Cookie.Update.empty))
    }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  object Ajax extends StrictLogging {
    import shipreq.webapp.base.protocol.ajax._

    private val authRequired =
      F pure ResponseCmd.StatusOnly.Forbidden.withoutCookieUpdate

    private val badRequest =
      ResponseCmd.StatusOnly.BadRequest.withoutCookieUpdate

    private def versionFailure(text: String) =
      Response(ResponseCmd.Text(StatusCode.NotImplemented, text), Cookie.Update.empty)

    private case class Route(handler: Handler, name: String, sessionRequired: Boolean)

    private type Handler = (Security.SessionToken[Option[Instant]], BinaryData, tracer.Span) => F[Response]

    private[this] val routeMap: Map[String, Route] = {
      val mutableRouteMap = new Url.Relative.MutableMap[Route]

      def _register[A](p: Protocol.Ajax[SafePickler], name: String, sessionRequired: Boolean)
                      (f: (Security.SessionToken[Option[Instant]], p.protocol.PreparedRequestType) => F[Response]): Unit = {
        assert(p.url.underlying startsWith Urls.ajaxRoot.underlying, s"${p.url} must start with ${Urls.ajaxRoot}")

        val serverVer = p.prepReq.codec.version

        // [/x/pub/lp : landingPage v1.0] Failed to decode ajax binary body.
        val logPrefix = s"[${p.url.relativeUrlNoTailSlash} : $name ${serverVer.verStr}] "

        import Version.ordering.mkOrderingOps

        def respondCVO(clientVer: Option[Version], r: => Response): Response =
          clientVer.fold(r)(respondCV(_, r))

        def respondCV(clientVer: Version, r: => Response): Response =
          if (clientVer > serverVer)
            versionFailure(s"Failed to parse protocol ${clientVer.verStr}. This server is still on ${serverVer.verStr}. We're probably in the middle of an upgrade. Please try again.")
          else
            r

        val h: Handler = (token, reqBin, span) => {
          val main: F[Response] =
            p.prepReq.codec.decode(reqBin) match {
              case \/-(req) =>
                f(token, req)
              case -\/(err) =>
                F.point {
                  err match {

                    case DecodingFailure.UnsupportedMajorVer(_, clientVer) =>
                      logger.warn(s"${logPrefix}Unsupported major version: ${clientVer.verStr}")
                      def serverAheadOfClient = versionFailure(s"Failed to parse protocol ${clientVer.verStr}. This server is on ${serverVer.verStr}.")
                      respondCV(clientVer, serverAheadOfClient)

                    case DecodingFailure.MagicNumberMismatch(_, client, server, clientVer) =>
                      logger.warn(s"${logPrefix}Magic-number mismatch: received ${client.hex}, expected ${server.hex}. (ClientVer:${clientVer.fold("?")(_.verNum)})")
                      respondCVO(clientVer, badRequest)

                    case DecodingFailure.InvalidVersion(_, major, minor) =>
                      logger.warn(s"${logPrefix}Invalid version: $major.$minor)")
                      badRequest

                    case DecodingFailure.ExceptionOccurred(_, e, clientVer) =>
                      logger.warn(s"${logPrefix}Failed to decode request. (ClientVer:${clientVer.fold("?")(_.verNum)})", e)
                      respondCVO(clientVer, badRequest)
                  }
                }
            }

          token.authenticatedUser match {
            case None => main
            case Some(u) =>
              for {
                _ <- tracer.alg.addAttrs(Trace.Attr.ShipReqUserId(u.id.value) :: Nil)(span)
                r <- main
              } yield r
          }
        }

        mutableRouteMap += (p.url -> Route(h, name, sessionRequired))
      }

      def responseCmd(p: Protocol.Ajax[SafePickler])(req: p.protocol.PreparedRequestType, out: p.protocol.ResponseType) = {
        val respCodec = p.responseProtocol(req).codec
        val outBin = respCodec.encode(out)
        ResponseCmd.Binary(StatusCode.OK, outBin)
      }

      def anon(p: Protocol.Ajax[SafePickler])(name: String, sessionRequired: Boolean, f: p.ServerSideFn[F]): Unit =
        _register(p, name, sessionRequired)((token, req) =>
          for {
            out   <- f(req)
            resCmd = responseCmd(p)(req, out)
            cu    <- security.sessionPersistIfNew(token)
          } yield Response(resCmd, cu)
        )

      type AnonOHandler[-A] = (Security.SessionToken[Any], ResponseCmd, A) => F[Response]

      def anonO[A](p: Protocol.Ajax[SafePickler])
                  (name: String,
                   sessionRequired: Boolean,
                   f: Security.SessionToken[Any] => p.ServerSideFnO[F, A])
                  (g: AnonOHandler[A]): Unit =
        _register(p, name, sessionRequired)((token, req) =>
          for {
            (out, a) <- f(token)(req)
            resCmd    = responseCmd(p)(req, out)
            res      <- g(token, resCmd, a)
          } yield res
        )

      def auth(p: Protocol.Ajax[SafePickler])(name: String, sessionRequired: Boolean, f: p.ServerSideFnI[F, User]): Unit =
        _register(p, name, sessionRequired)((token, req) =>
          token.authenticatedUser match {
            case Some(user) =>
              for {
                out   <- f(user, req)
                resCmd = responseCmd(p)(req, out)
                cu    <- security.sessionPersistIfNew(token)
              } yield Response(resCmd, cu)
            case None =>
              authRequired
          }
        )

      val useNewToken: AnonOHandler[Option[Security.SessionToken[Unit]]] =
        (oldToken, res, newToken) =>
          security.sessionPersist(newToken getOrElse oldToken).map(Response(res, _))

      val dynamicAuth: AnonOHandler[Permission] =
        (_, res, perm) =>
          perm match {
            case Allow => F pure Response(res, Cookie.Update.empty)
            case Deny  => authRequired
          }

      // Register endpoints
      anonO(CommonProtocols   .Login            .ajax)("login"            , false, common   .ajaxLogin            )(useNewToken)
      anonO(CommonProtocols   .ReportClientError.ajax)("reportClientError", false, common   .ajaxReportClientError)(dynamicAuth)
      anonO(CommonProtocols   .SubmitFeedback   .ajax)("submitFeedback"   , false, common   .ajaxSubmitFeedback   )(dynamicAuth)
      anon (PublicSpaProtocols.LandingPage      .ajax)("landingPage"      , true , publicSpa.ajaxLandingPage      )
      anon (PublicSpaProtocols.Register1        .ajax)("register1"        , true , publicSpa.ajaxRegister1        )
      anonO(PublicSpaProtocols.Register2        .ajax)("register2"        , true , publicSpa.ajaxRegister2        )(useNewToken)
      anon (PublicSpaProtocols.ResetPassword1   .ajax)("resetPassword1"   , true , publicSpa.ajaxResetPassword1   )
      anon (PublicSpaProtocols.ResetPassword2   .ajax)("resetPassword2"   , true , publicSpa.ajaxResetPassword2   )
      auth (HomeSpaProtocols  .CreateProject    .ajax)("createProject"    , true , homeSpa  .ajaxCreateProject    )

      mutableRouteMap.toMapNoHeadSlash
    }

    private val notFound: F[Response] =
      F pure Response(ResponseCmd.StatusOnly.NotFound, Cookie.Update.empty)

    val candidate: Url.Relative => Boolean =
      Urls.ajaxRoot.isEqualToOrParentOf

    val routes: Request ?=> F[Response] =
      when(r => candidate(r.path)) { implicit req =>

        routeMap.get(req.path.relativeUrlNoHeadSlash) match {
          case Some(route) =>

            def respond(span: tracer.Span): F[Response] = {

              def respondWithSession(s: Security.SessionToken[Option[Instant]]) =
                if (req.method eq Post)
                  req.body.value match {
                    case Some(reqBin) => route.handler(s, reqBin, span)
                    case None         => F pure ResponseCmd.StatusOnly.BadRequest.withoutCookieUpdate
                  }
                else
                  F pure ResponseCmd.StatusOnly.MethodNotAllowed.withoutCookieUpdate

              if (route.sessionRequired)
                requireSession(s => respondWithSession(s.copy(expiry = Some(s.expiry))))
              else
                security.sessionRestoreOrCreate(req.cookie).flatMap(respondWithSession)
            }

            for {
              _ <- metrics.setServerSideProcName(route.name)
              r <- tracer.serverSideProc(route.name, req.real, req.path)(span => respond(span))
            } yield r

          case None =>
            notFound
        }
      }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Ops & Diagnostics

  object Ops {
    import shipreq.taskman.api.TaskId

    private def response(cmd: ResponseCmd): Response =
      Response(cmd, Cookie.Update.empty)

    private val notFoundSecure: F[Response] =
      security.protect( // prevent response-time hacking to discover endpoints (meaning ops URLs)
        F pure ResponseCmd.StatusOnly.NotFound.withoutCookieUpdate)

    private def endpoint(method: Method, url: Url.Relative)(f: Request => F[Response]): Request ?=> F[Response] =
      whenUrlIs(url) { implicit req =>
        traceUrl(req.path,
          req.param(opsSecretKey) match {
            case Some(key) if key ==* opsSecretValue.value && req.method ==* method => security.protect(f(req))
            case _                                                                  => notFoundSecure
          }
        )
      }

    private def whenValid[A](fa: F[ErrorMsg \/ A])(f: A => Response): F[Response] =
      fa.map {
        case \/-(a) => f(a)
        case -\/(e) => response(ResponseCmd.Text(StatusCode.BadRequest, e.value))
      }

    private def jsonResponse(r: OpsEndpoints.HasJson): Response =
      response(ResponseCmd.Json(StatusCode.OK, r.toJson))

    /** Return a static 200.
      * Useful to test that the web-server is up and serving requests.
      * Used for container health-checks.
      */
    private val ok: Request ?=> F[Response] = {
      val r = response(ResponseCmd.Text(StatusCode.OK, "OK."))
      get(Url.Relative("ok"), F pure r)
    }

    /** API for invoking the first part of the registration process
      * (regardless of whether public registrations are enabled or not).
      */
    private val register1: Request ?=> F[Response] =
      endpoint(Post, Url.Relative("register1"))(req =>
        parseParams(req.param("email"))(email =>
          whenValid(publicSpa.apiRegister1(email))(id =>
            response(ResponseCmd.Json(StatusCode.OK, Json.obj("taskId" -> id.value.asJson))))))

    private val statsDb: Request ?=> F[Response] =
      endpoint(Post, Url.Relative("stats/db"))(
        Function const ops.dbStats.map(jsonResponse))

    private val statsUsers: Request ?=> F[Response] =
      endpoint(Post, Url.Relative("stats/users"))(
        Function const ops.userStats.map(jsonResponse))

    /** API to inspect the status of a Taskman message. */
    private val task: Request ?=> F[Response] =
      endpoint(Post, Url.Relative("task"))(req =>
        parseParams(req.param("id") flatMap ParseLong.unapply)(id =>
          ops.taskmanMsgStatus(TaskId(id)).map {
            case Some(r) => jsonResponse(r)
            case None    => response(ResponseCmd.StatusOnly.NotFound)
          }
        )
      )

    private val getProjectEvents: Request ?=> F[Response] =
      endpoint(Post, Url.Relative("project/events"))(req =>
        parseParams(req.param("id") flatMap ParseLong.unapply)(id =>
          ops.getProjectEvents(ProjectId(id)).map(response)
        )
      )

    private val createProject: Request ?=> F[Response] =
      endpoint(Post, Url.Relative("project/create"))(req =>
        parseParams(
          for {
            user   <- req.param("user")
            events <- req.param("events")
          } yield (Username.orEmail(user), events)
        ){ case (user, events) =>
          ops.createProject(user, events).map(response)
        }
      )

    private val testSendMail: Request ?=> F[Response] =
      endpoint(Post, Url.Relative("test-sendmail"))(req =>
        parseParams(req.param("email"))(email =>
          whenValid(ops.sendMail(email))(
            jsonResponse)))

    private def innerRoutes: Request ?=> F[Response] =
      ok | register1 | statsDb | statsUsers | task | testSendMail | getProjectEvents | createProject

    private val fallback: Request => F[Response] =
      _ => notFoundSecure

    val candidate: Url.Relative => Boolean =
      opsRoot.isEqualToOrParentOf

    val routes: Request ?=> F[Response] =
      when(r => candidate(r.path))(scope(opsRoot, innerRoutes).withFallback(fallback))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Other

  /** FOR UNIT-TESTS ONLY */
  private val unitTestLogin: Request ?=> F[Response] =
    whenUrlIs(unitTestLoginUrl){ implicit req =>
      onMethod(Post)(security.protect(
        parseParams(
          for {
            u <- req.param("user")
            p <- req.param("pass")
          } yield (Username.orEmail(u), PlainTextPassword(p))
        ) { case (u, p) =>
          security.attemptLogin(u, p).flatMap {
            case Some(u) => security.sessionPersist(Security.SessionToken.anonymous().login(u)).map(Response(ResponseCmd.StatusOnly.OK, _))
            case None    => F pure ResponseCmd.StatusOnly.Forbidden.withoutCookieUpdate
          }
        }
      ))
    }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  def allLogic(testMode: Boolean): Request => F[Response] =
    ( Main.routes
    | Ajax.routes
    | Ops.routes
    | Option.when(testMode)(unitTestLogin)
    ).withFallback(Main.fallback)

  def all(testMode: Boolean): RealReq => F[Response] =
    allLogic(testMode = testMode)
      .compose(readRealReq)

}
