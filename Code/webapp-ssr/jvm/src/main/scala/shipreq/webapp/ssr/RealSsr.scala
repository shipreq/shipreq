package shipreq.webapp.ssr

import japgolly.scalagraal._
import japgolly.scalagraal.util.ReactSsr

object RealSsr {
  import GraalBoopickle._
  import GraalJs._
  import SsrSharedData._

  val setup: Expr[Unit] =
    ReactSsr.Setup(Expr.requireFileOnClasspath("webapp-ssr-deps.js")) >>
      Expr.requireFileOnClasspath("webapp-ssr.js").void

  val renderPublic: PublicInitData => Expr[String] =
    Expr.fn1[PublicInitData](SsrJsFunctionManifest.PublicLoader).compile(_.asString)

  val renderHomeSpaLoader: HomeSpaLoaderData => Expr[String] =
    Expr.fn1[HomeSpaLoaderData](SsrJsFunctionManifest.HomeSpaLoader).compile(_.asString)

  val renderProjectSpaLoader: ProjectSpaLoaderData => Expr[String] =
    Expr.fn1[ProjectSpaLoaderData](SsrJsFunctionManifest.ProjectSpaLoader).compile(_.asString)

}
