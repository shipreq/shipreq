package shipreq.webapp.client.public.spa

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.extra.router.{RouterCtl => _, _}

import shipreq.base.util.Url
import shipreq.base.util.univeq._
import shipreq.webapp.base.{AnalyticsConfig, Urls, WebappConfig}
import shipreq.webapp.base.Urls.PublicSpaRoute
import shipreq.webapp.base.data.VerificationToken
import shipreq.webapp.base.lib.BaseReusability._
import shipreq.webapp.base.util.GoogleAnalytics

sealed trait Page {
  val pageTitle: List[String]
}

object Page {

  final case class Static(route: PublicSpaRoute.Static) extends Page {
    val linkTitle: String =
      route match {
        case PublicSpaRoute.Home           => "Home"
        case PublicSpaRoute.Login          => "Login"
        case PublicSpaRoute.Privacy        => "Privacy"
        case PublicSpaRoute.Register1      => "Register"
        case PublicSpaRoute.TermsOfService => "Terms"
      }

    override val pageTitle: List[String] =
      route match {
        case PublicSpaRoute.Home => Nil
        case _                   => linkTitle :: Nil
      }
  }

  final case class Token(route: PublicSpaRoute.NeedsToken, token: VerificationToken) extends Page {
    override val pageTitle: List[String] =
      route match {
        case PublicSpaRoute.Register2     => "Register"       :: Nil
        case PublicSpaRoute.ResetPassword => "Reset Password" :: Nil
      }
  }

  final case class LoginFrom(url: Url.Relative) extends Page {
    override val pageTitle: List[String] =
      "Login" :: Nil
  }

  implicit def equality: UnivEq[Page] = UnivEq.derive
  implicit def reusability: Reusability[Page] = Reusability.byUnivEq
  val static = PublicSpaRoute.static.map(Static)

  val Home           = Static(PublicSpaRoute.Home)
  val Login          = Static(PublicSpaRoute.Login)
  val Privacy        = Static(PublicSpaRoute.Privacy)
  val Register1      = Static(PublicSpaRoute.Register1)
  val TermsOfService = Static(PublicSpaRoute.TermsOfService)
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object Routes {

  def routerConfig(spa: PublicSpa) =
    RouterConfigDsl[Page].buildConfig { dsl =>
      import dsl._

      def render(page: Page, r: RouterCtl) =
        spa.Component(PublicSpa.Props(page, r))

      val userIsLoggedIn = spa.initData.loggedInUser.isDefined

      val loginFrom = {
        val route: StaticDsl.Route[Page.LoginFrom] =
          (Page.Login.route.url.relativeUrl / remainingPath)
            .xmap(Url.Relative(_))(_.relativeUrlNoHeadSlash)
            .xmap(Page.LoginFrom)(_.url)
        val action: Page.LoginFrom => Action = p =>
          if (userIsLoggedIn)
            redirectToPath(p.url.relativeUrl)(SetRouteVia.WindowLocation)
          else
            renderR(render(p, _))
        dynamicRouteCT(route) ~> action
      }

      val staticRoutes =
        Page.static.map { p =>
          val url = p.route.url
          val route: StaticDsl.Route[Unit] =
            if (url.isRoot) dsl.root else url.relativeUrl
          // This logic is mirrored in DispatchLogic
          val action: Action =
            if (userIsLoggedIn && p.route ==* PublicSpaRoute.Login)
              redirectToPath(Urls.memberHome.relativeUrl)(SetRouteVia.WindowLocation)
            else
              renderR(render(p, _))
          staticRoute(route, p) ~> action
        }.reduce(_ | _)

      val tokenRoutes =
        PublicSpaRoute.needsToken.map { r =>
          val tokenUrl = remainingPath.xmap(s => Page.Token(r, VerificationToken(s)))(_.token.value)
          dynamicRoute(r.url.prefix.relativeUrl / tokenUrl) {
            case p@ Page.Token(r2, _) if r ==* r2 => p
          } ~> dynRenderR(render(_, _))
        }.reduce(_ | _)

      (removeQuery | removeTrailingSlashes | loginFrom | staticRoutes | tokenRoutes)
        .notFound(redirectToPage(Page.Home)(SetRouteVia.HistoryReplace))
        .setTitle(p => WebappConfig.makePageTitle(p.pageTitle: _*))
        .onPostRender(trackPage)
        .verify(
          Page.LoginFrom(Url.Relative("/blah")),
          Page.static.whole ++
            PublicSpaRoute.needsToken.whole.map(Page.Token(_, VerificationToken("abcd"))): _*)
    }

    private val trackPage: (Option[Page], Page) => Callback = {
      val path: Page => Url.Relative = {
        case Page.Static(route)   => route.url
        case Page.LoginFrom(_)    => PublicSpaRoute.Login.url
        case Page.Token(route, _) => route.prefix / "<token>"
      }
      GoogleAnalytics.onRouteChange(_, _)(path)
    }
}
