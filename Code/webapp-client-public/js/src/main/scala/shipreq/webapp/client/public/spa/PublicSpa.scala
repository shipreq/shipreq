package shipreq.webapp.client.public.spa

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.MonocleReact._
import shipreq.webapp.base.Urls.PublicSpaRoute
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.protocol._
import shipreq.webapp.client.public.{PublicSpaProtocols => P}

object PublicSpa {
  final case class Props(page: Page, routerCtl: RouterCtl)
}

final class PublicSpa(initData: P.InitData, cp: ClientProtocol) {
  import PublicSpa._

  val Component = ScalaComponent.builder[Props]("Root")
    .initialState(State.init)
    .renderBackend[Backend]
    .build

  final class Backend($: BackendScope[Props, State]) {
    import shipreq.webapp.client.public.pages._

    val sspLandingPage    = ServerSideProcInvoker(cp, initData.landingPage)
    val sspLogin          = ServerSideProcInvoker(cp, initData.login)
    val sspResetPassword1 = ServerSideProcInvoker(cp, initData.resetPassword1)
    val sspResetPassword2 = ServerSideProcInvoker(cp, initData.resetPassword2)
    val sspRegister1      = ServerSideProcInvoker(cp, initData.register1)
    val sspRegister2      = ServerSideProcInvoker(cp, initData.register2)

    val awLandingPage = AsyncFeature.Write.D0.init($.zoomStateL(State.landingPage ^|-> LandingPage.State.async))
    val awLogin       = AsyncFeature.Write.D0.init($.zoomStateL(State.login       ^|-> Login      .State.async))
    val awRegister1   = AsyncFeature.Write.D0.init($.zoomStateL(State.register1   ^|-> Register1  .State.async))

    def render(p: Props, s: State): VdomElement = {

      val content: VdomElement =
        p.page match {

          case Page.Static(PublicSpaRoute.Home) =>
            val ss = StateSnapshot.zoomL(State.landingPage)(s).setStateVia($)
            LandingPage.Props(ss, awLandingPage, sspLandingPage).render

          case Page.Static(PublicSpaRoute.Login) =>
            val ss = StateSnapshot.zoomL(State.login)(s).setStateVia($)
            Login.Props(ss, awLogin, sspLogin, sspResetPassword1).render

          case Page.Token(PublicSpaRoute.ResetPassword, token) =>
            ResetPassword.Props(token, sspResetPassword2).render

          case Page.Static(PublicSpaRoute.Register1) =>
            val ss = StateSnapshot.zoomL(State.register1)(s).setStateVia($)
            Register1.Props(initData.allowRegister, p.routerCtl, ss, awRegister1, sspRegister1).render

          case Page.Static(PublicSpaRoute.Privacy) =>
            Legal.Privacy(p.routerCtl)

          case Page.Static(PublicSpaRoute.TermsOfService) =>
            Legal.TermsOfService(p.routerCtl)
        }

      Layout.Component(Layout.Props(p.page, p.routerCtl, content))
    }
  }

}