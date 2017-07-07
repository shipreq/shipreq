package shipreq.webapp.client.public.root

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra._
import shipreq.webapp.base.PublicUrls.SpaRoute
import shipreq.webapp.base.protocol.ClientProtocol

object Root {
  final case class Props(page: Page, routerCtl: RouterCtl)
}

final class Root(cp: ClientProtocol) {
  import Root._

  val Component = ScalaComponent.builder[Props]("Root")
    .initialState(State.init)
    .renderBackend[Backend]
    .build

  final class Backend($: BackendScope[Props, State]) {
    def render(p: Props, s: State): VdomElement = {
      import shipreq.webapp.client.public.pages._

      val content: VdomElement =
        p.page match {
          case Page.Static(SpaRoute.Home) => LandingPage.Props().render
        }

      Layout.Component(Layout.Props(p.page, p.routerCtl, content))
    }
  }

}