package shipreq.webapp.ssr

import cats.instances.either._
import com.typesafe.scalalogging.StrictLogging
import japgolly.scalagraal.ExprError
import japgolly.scalagraal.util._
import shipreq.base.util.{Permission, Url}
import shipreq.webapp.base.user.Username
import shipreq.webapp.base.data.Project
import scalaz.Applicative

/** SSR interpreter that
  *
  * - is minimal in that it only provides SSR for / and /project/id
  * - is minimal in that it only runs SSR on startup uses [[CacheAndReplace]] when serving
  */
final class SsrMinimal[F[_]]()(implicit F: Applicative[F]) extends SsrAlgebra[F] with StrictLogging {
  import SsrAlgebra._
  import SsrSharedData._

  override def prepare(baseUrl: Url.Absolute.Base,
                       publicRegistration: Permission): F[Prepared[F]] =
    RealSsr.withNewCtx { ctx =>
      val fxNoOutput: F[Output] = F.pure(None)

      implicit def cacheAndReplaceParamUsername: CacheAndReplace.Param[Username] =
        CacheAndReplace.Param(Username.apply)(_.value)

      def fail1(name: String, e: ExprError): Any => F[Output] = {
        logger.warn(s"Failed to pre-compile $name SSR.", e)
        _ => fxNoOutput
      }

      // ===============================================================================================================

      val public: Public[F] = {

        def render(u: Option[Username]) =
          ctx.eval(RealSsr.renderPublic(PublicInitData(publicRegistration, u)))

        val cached =
          for {
            _    <- ctx.eval(ReactSsrUtil.setUrl(baseUrl.value))
            anon <- render(None)
            user <- CacheAndReplace.compile1((u: Username) => render(Some(u)))
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
      }

      // ===============================================================================================================

      val home: HomeSpaLoader[F] = {

        def render(u: Username) =
          ctx.eval(RealSsr.renderHomeSpaLoader(HomeSpaLoaderData(u)))

        CacheAndReplace.compile1(render) match {
          case Right(t) => i => F.pure(Some(Html(t(i.username))))
          case Left(e)  => fail1("home SPA loader", e)
        }
      }

      // ===============================================================================================================

      val project: ProjectSpaLoader[F] = {

        def render(u: Username, p: Project.Name) =
          ctx.eval(RealSsr.renderProjectSpaLoader(ProjectSpaLoaderData(u, p)))

        CacheAndReplace.compile2(render) match {
          case Right(t) => i => F.pure(Some(Html(t(i.username, i.projectName))))
          case Left(e)  => fail1("project SPA loader", e)
        }
      }

      // ===============================================================================================================

      Prepared(public, home, project)
    }
}
