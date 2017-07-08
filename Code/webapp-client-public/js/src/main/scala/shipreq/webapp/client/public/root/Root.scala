package shipreq.webapp.client.public.root

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.MonocleReact._
import shipreq.webapp.base.PublicUrls.SpaRoute
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.protocol._
import shipreq.webapp.client.public.{PublicSpaProtocols => P}

object Root {
  final case class Props(page: Page, routerCtl: RouterCtl)
}

final class Root(initData: P.InitData, cp: ClientProtocol) {
  import Root._

  val Component = ScalaComponent.builder[Props]("Root")
    .initialState(State.init)
    .renderBackend[Backend]
    .build

  final class Backend($: BackendScope[Props, State]) {
    import shipreq.webapp.client.public.pages._

    val landingPageAW = AsyncFeature.Write.D0.init($.zoomStateL(State.landingPage ^|-> LandingPage.State.async))
    val landingPageIO = ServerSideProcInvoker(initData.landingPage, cp)

    def render(p: Props, s: State): VdomElement = {

      val content: VdomElement =
        p.page match {

          case Page.Static(SpaRoute.Home) =>
            val ss = StateSnapshot.zoomL(State.landingPage)(s).setStateVia($)
            LandingPage.Props(ss, landingPageAW, landingPageIO).render

        }

      Layout.Component(Layout.Props(p.page, p.routerCtl, content))
    }
  }

}