package shipreq.webapp.client.public.root

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.extra.router.{RouterCtl => _, _}
import japgolly.scalajs.react.vdom.Implicits._
import shipreq.base.util.univeq._
import shipreq.webapp.base.WebappConfig
import shipreq.webapp.base.lib.BaseReusability._

sealed trait Page {
  def pageTitle: List[String]
}
object Page {

  sealed abstract class Static(val linkTitle: String,
                               val pageTitle: List[String]) extends Page {
    def this(title: String) {
      this(title, title :: Nil)
    }
  }

  case object LandingPage    extends Static("Home", Nil)
  case object Login          extends Static("Login")
  case object Register       extends Static("Register")
  case object Privacy        extends Static("Privacy")
  case object TermsOfService extends Static("Terms")

  implicit def equality: UnivEq[Page] = UnivEq.derive
  implicit def reusability: Reusability[Page] = Reusability.byUnivEq
  val values = AdtMacros.adtValues[Page]
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

      ( staticPage(dsl.root       , Page.LandingPage   )
      | staticPage("/login"       , Page.Login         )
      | staticPage("/register"    , Page.Register     )
      | staticPage("/privacy"     , Page.Privacy       )
      | staticPage("/tos"         , Page.TermsOfService)
      | trimSlashes
      ).notFound(redirectToPage(Page.LandingPage)(Redirect.Replace))
        .setTitle(p => WebappConfig.makePageTitle(p.pageTitle: _*))
        .verify(Page.values.head, Page.values.tail: _*)
    }
}
