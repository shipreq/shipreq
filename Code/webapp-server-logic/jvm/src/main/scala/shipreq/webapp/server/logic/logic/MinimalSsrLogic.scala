package shipreq.webapp.server.logic.logic

import cats.effect.Sync
import cats.syntax.all._
import com.typesafe.scalalogging.StrictLogging
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalagraal._
import japgolly.scalagraal.js._
import japgolly.scalagraal.util._
import shipreq.base.ops.Trace
import shipreq.base.util.{Permission, Url}
import shipreq.webapp.base.config.AssetManifest
import shipreq.webapp.base.data.Username
import shipreq.webapp.server.logic.algebra.Server
import shipreq.webapp.ssr._

/** SSR interpreter that
  *
  * - is minimal in that it only provides SSR for / and /project/id
  * - is minimal in that it only runs SSR on startup uses [[StrFnCache]] when serving
  */
final class MinimalSsrLogic[F[_]]()(implicit F: Sync[F],
                                    am: AssetManifest,
                                    trace: Trace.Algebra[F],
                                    svr: Server.Time[F]) extends SsrAlgebra[F] with StrictLogging {
  import GraalJs._
  import SsrAlgebra._
  import SsrSharedData._

  private def logDuration[A](name: String)(fa: F[A]): F[A] =
    for {
      (a, dur) <- svr.measureDuration(fa)
      _        <- F.delay(logger.info(s"$name completed in ${dur.conciseDesc}."))
    } yield a

  private def logAndTrace[A](name: String)(fa: F[A]): F[A] =
    trace.newSpan("SSR:" + name)(_ =>
      logDuration("SSR:" + name)(fa))

  private def assertOk[A](result: Expr.Result[A]): F[A] =
    F.delay {
      result match {
        case Right(a) => a
        case Left(e) => throw e
      }
    }

  private def withCtx[A](f: GraalContext => F[A]): F[A] =
    for {
      ctx <- F.delay(GraalContext.fixedContext())
      res <- logAndTrace("setup")(F.delay(ctx.eval(RealSsr.setup)))
      _   <- assertOk(res)
      a   <- f(ctx)
      _   <- F.delay(ctx.close())
    } yield a

  override def prepare(baseUrl: Url.Absolute.Base,
                       publicRegistration: Permission): F[Prepared[F]] = {

    implicit val strFnCacheRouteUrlRelative: StrFnCacheRoute[Url.Relative] =
      StrFnCacheRoute.apply1(Url.Relative.apply)(_.relativeUrlNoHeadSlash)

    implicit val strFnCacheParamAssetManifest: StrFnCacheParam[AssetManifest] =
      StrFnCacheParam.const(am)

    implicit val strFnCacheParamUsername: StrFnCacheParam[Username] =
      StrFnCacheParam.apply1(Username.apply)(_.value)

    implicit val strFnCacheParamHomeSpaLoaderData: StrFnCacheParam[HomeSpaLoaderData] =
      StrFnCacheParam.apply2(HomeSpaLoaderData.apply)(d => (d.username, d.assetManifest))

    implicit val strFnCacheParamProjectSpaLoaderData: StrFnCacheParam[ProjectSpaLoaderData] =
      StrFnCacheParam.apply3(ProjectSpaLoaderData.apply)(d => (d.username, d.projectName, d.assetManifest))

    def wrap1[A](name: String, f: A => Expr.Result[String]): A => F[Output] =
      a => F.delay {
        f(a) match {
          case Right(s) =>
            Some(Html(s))
          case Left(e) =>
            logger.warn(s"Failed to execute $name SSR.", e)
            None
        }
      }

    def wrapOpt2[A, B](name: String, f: (A, B) => Expr.Result[Option[String]]): (A, B) => F[Output] =
      (a, b) => F.delay {
        f(a, b) match {
          case Right(o) =>
            o.map(Html(_))
          case Left(e) =>
            logger.warn(s"Failed to execute $name SSR.", e)
            None
        }
      }

    val prep = withCtx { ctx =>

      // ===============================================================================================================

      val public: F[Public[F]] = logAndTrace("public")(F.delay {

        def render(path: Url.Relative, u: Option[Username]): Expr.Result[String] =
          ctx.eval(ReactSsr.setUrl((baseUrl / path).absoluteUrl)) >>
            ctx.eval(RealSsr.renderPublic(PublicInitData(publicRegistration, u, am)))

        val cache = StrFnCache.withRouteWhitelist(render)(
          Url.Relative.root,
        )

        wrapOpt2("public SPA", cache)
      })

      // ===============================================================================================================

      val home: F[HomeSpaLoader[F]] = logAndTrace("home")(F.delay {

        def render(d: HomeSpaLoaderData): Expr.Result[String] =
          ctx.eval(RealSsr.renderHomeSpaLoader(d))

        wrap1("home SPA loader", StrFnCache(render))
      })

      // ===============================================================================================================

      val project: F[ProjectSpaLoader[F]] = logAndTrace("project")(F.delay {

        def render(d: ProjectSpaLoaderData): Expr.Result[String] =
          ctx.eval(RealSsr.renderProjectSpaLoader(d))

        wrap1("project SPA loader", StrFnCache(render))
      })

      // ===============================================================================================================

      F.map3(public, home, project)(Prepared.apply[F])
    }

    logDuration("SSR preparation")(prep)
  }
}
