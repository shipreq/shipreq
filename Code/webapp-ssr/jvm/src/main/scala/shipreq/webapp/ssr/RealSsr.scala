package shipreq.webapp.ssr

import japgolly.scalagraal._
import japgolly.scalagraal.util.ReactSsrUtil

object RealSsr {
  import GraalBoopickle._
  import GraalJs._
  import SsrSharedData._

  val setup: Expr[Unit] =
    ReactSsrUtil.Setup(
      Expr.requireFileOnClasspath("webapp-ssr-deps.js"),
      Expr.requireFileOnClasspath("webapp-ssr.js"),
    )

  val renderPublic: PublicInitData => Expr[String] =
    Expr.compileFnCall1[PublicInitData](SsrJsFunctionManifest.Public)(_.asString)

  val renderHomeSpaLoader: HomeSpaLoaderData => Expr[String] =
    Expr.compileFnCall1[HomeSpaLoaderData](SsrJsFunctionManifest.HomeSpaLoader)(_.asString)

  val renderProjectSpaLoader: ProjectSpaLoaderData => Expr[String] =
    Expr.compileFnCall1[ProjectSpaLoaderData](SsrJsFunctionManifest.ProjectSpaLoader)(_.asString)

}
