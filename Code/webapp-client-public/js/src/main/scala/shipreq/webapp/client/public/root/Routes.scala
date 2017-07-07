package shipreq.webapp.client.public.root

import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.extra.router.{RouterCtl => _, _}
import japgolly.scalajs.react.vdom.Implicits._
import shipreq.base.util.univeq._
import shipreq.webapp.base.PublicUrls.SpaRoute
import shipreq.webapp.base.WebappConfig
import shipreq.webapp.base.lib.BaseReusability._

sealed trait Page {
  val route: SpaRoute
  val linkTitle: String
  val pageTitle: List[String]
}
object Page {

  final case class Static(route: SpaRoute.Static) extends Page {
    override val linkTitle: String =
      route match {
        case SpaRoute.Home           => "Home"
        case SpaRoute.Login          => "Login"
        case SpaRoute.Privacy        => "Privacy"
        case SpaRoute.Register1      => "Register"
        case SpaRoute.TermsOfService => "Terms"
      }

    val pageTitle: List[String] =
      route match {
        case SpaRoute.Home => Nil
        case _             => linkTitle :: Nil
      }
  }

  implicit def equality: UnivEq[Page] = UnivEq.derive
  implicit def reusability: Reusability[Page] = Reusability.byUnivEq
  val static = SpaRoute.static.map(Static)

  val Home           = Static(SpaRoute.Home)
  val Login          = Static(SpaRoute.Login)
  val Privacy        = Static(SpaRoute.Privacy)
  val Register1      = Static(SpaRoute.Register1)
  val TermsOfService = Static(SpaRoute.TermsOfService)
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object Routes {

  def routerConfig(rootInstance: Root) =
    RouterConfigDsl[Page].buildConfig { dsl =>
      import dsl._

      def render(page: Page, r: RouterCtl) =
        rootInstance.Component(Root.Props(page, r))

//      val dynPage = dynRenderR((page: Page, r) => render(page, r))

      def staticPage(route: StaticDsl.Route[Unit], page: Page) =
        staticRoute(route, page) ~> renderR(r => render(page, r))

      val staticRoutes =
        Page.static.map { p =>
          val url = p.route.url
          staticPage(if (url.isRoot) dsl.root else url.relativeUrl, p)
        }.reduce(_ | _)

      (staticRoutes | trimSlashes)
        .notFound(redirectToPage(Page.Home)(Redirect.Replace))
        .setTitle(p => WebappConfig.makePageTitle(p.pageTitle: _*))
        .verify(Page.static.head, Page.static.tail: _*)
    }
}
