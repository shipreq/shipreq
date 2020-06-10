package shipreq.webapp.server.logic

import cats.instances.either._
import com.typesafe.scalalogging.StrictLogging
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalagraal._
import japgolly.scalagraal.util._
import scalaz.Monad
import scalaz.syntax.monad._
import shipreq.base.ops.Trace
import shipreq.base.util.{Permission, Url}
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.user.Username
import shipreq.webapp.ssr._

/** SSR interpreter that
  *
  * - is minimal in that it only provides SSR for / and /project/id
  * - is minimal in that it only runs SSR on startup uses [[CacheAndReplace]] when serving
  */
final class MinimalSsr[F[_]]()(implicit F: Monad[F],
                               trace: Trace.Algebra[F],
                               svr: Server.Time[F]) extends SsrAlgebra[F] with StrictLogging {
  import GraalJs._
  import SsrAlgebra._
  import SsrSharedData._

  private def logDuration[A](name: String)(fa: F[A]): F[A] =
    for {
      (a, dur) <- svr.measureDuration(fa)
      _        <- F.point(logger.info(s"$name completed in ${dur.conciseDesc}."))
    } yield a

  private def logAndTrace[A](name: String)(fa: F[A]): F[A] =
    trace.newSpan("SSR:" + name)(_ =>
      logDuration("SSR:" + name)(fa))

  private def assertOk[A](result: Expr.Result[A]): F[A] =
    F.point {
      result match {
        case Right(a) => a
        case Left(e) => throw e
      }
    }

  private def withCtx[A](f: ContextSync => F[A]): F[A] =
    for {
      ctx <- F.point(ContextSync.fixedContext())
      res <- logAndTrace("setup")(F.point(ctx.eval(RealSsr.setup)))
      _   <- assertOk(res)
      a   <- f(ctx)
      _   <- F.point(ctx.close())
    } yield a

  override def prepare(baseUrl: Url.Absolute.Base,
                       publicRegistration: Permission): F[Prepared[F]] = {

    val fxNoOutput: F[Output] = F.pure(None)

    implicit def cacheAndReplaceParamUsername: CacheAndReplace.Param[Username] =
      CacheAndReplace.Param(Username.apply)(_.value)

    def fail1(name: String, e: ExprError): Any => F[Output] = {
      logger.warn(s"Failed to pre-compile $name SSR.", e)
      _ => fxNoOutput
    }

    val prep = withCtx { ctx =>

      // ===============================================================================================================

      val public: F[Public[F]] = logAndTrace("public")(F.point {

        def render(u: Option[Username]): Expr.Result[String] =
          ctx.eval(RealSsr.renderPublic(PublicInitData(publicRegistration, u)))

        val cached =
          for {
            _    <- ctx.eval(ReactSsr.setUrl(baseUrl.value))
            anon <- render(None)
            user <- CacheAndReplace.compileF1((u: Username) => render(Some(u)))
          } yield (Html(anon), user.andThen(Html.apply))

        cached match {
          case Right((anon, userFn)) =>
            val fxAnon = F.pure(Option(anon))
            (url, username) =>
              if (url.isRoot)
                username.fold(fxAnon)(u => F.pure(Some(userFn(u))))
              else
                fxNoOutput

          case Left(e) =>
            logger.warn("Failed to pre-compile public SPA SSR.", e)
            (_, _) => fxNoOutput
        }
      })

      // ===============================================================================================================

      val home: F[HomeSpaLoader[F]] = logAndTrace("home")(F.point {

        def render(u: Username): Expr.Result[String] =
          ctx.eval(RealSsr.renderHomeSpaLoader(HomeSpaLoaderData(u)))

        CacheAndReplace.compileF1(render) match {
          case Right(t) => i => F.pure(Some(Html(t(i.username))))
          case Left(e)  => fail1("home SPA loader", e)
        }
      })

      // ===============================================================================================================

      val project: F[ProjectSpaLoader[F]] = logAndTrace("project")(F.point {

        def render(u: Username, p: Project.Name): Expr.Result[String] =
          ctx.eval(RealSsr.renderProjectSpaLoader(ProjectSpaLoaderData(u, p)))

        CacheAndReplace.compileF2(render) match {
          case Right(t) => i => F.pure(Some(Html(t(i.username, i.projectName))))
          case Left(e)  => fail1("project SPA loader", e)
        }
      })

      // ===============================================================================================================

      F.apply3(public, home, project)(Prepared.apply[F])
    }

    logDuration("SSR preparation")(prep)
  }
}
