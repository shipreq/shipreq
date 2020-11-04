package shipreq.webapp.client.public.spa

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import shipreq.base.util.Url
import shipreq.webapp.base.config.AssetManifest
import shipreq.webapp.base.config.Urls.PublicSpaRoute
import shipreq.webapp.base.feature.{AsyncFeature, ErrorHandlingFeature}
import shipreq.webapp.base.protocol.ajax._
import shipreq.webapp.client.public.pages._
import shipreq.webapp.client.public.{PublicSpaEntryPoint, PublicSpaProtocols}

object PublicSpa {
  final case class Props(page: Page, routerCtl: RouterCtl, am: AssetManifest)

  @Lenses
  final case class State(landingPage: LandingPage.State,
                         login      : Login      .State,
                         register1  : Register1  .State)

  object State {

    val recorder = ErrorHandlingFeature.StateRecorder[State]

    def init: CallbackTo[State] =
      for {
        login <- Login.State.init
      } yield State(
        landingPage = LandingPage.State.init,
        login       = login,
        register1   = Register1.State.init,
      )
  }
}

final class PublicSpa(val initData: PublicSpaEntryPoint.InitData, ajax: AjaxClient.Binary) {
  import PublicSpa._

  val Component = ScalaComponent.builder[Props]
    .initialStateCallback(State.recorder.getOrElseCB(State.init))
    .renderBackend[Backend]
    .build

  final class Backend($: BackendScope[Props, State]) {

    val sspLogin          = ajax.invoker(CommonProtocols.Login.ajax)
    val sspLandingPage    = ajax.invoker(PublicSpaProtocols.LandingPage.ajax).mergeFailure
    val sspResetPassword1 = ajax.invoker(PublicSpaProtocols.ResetPassword1.ajax)
    val sspResetPassword2 = ajax.invoker(PublicSpaProtocols.ResetPassword2.ajax).mergeFailure
    val sspRegister1      = ajax.invoker(PublicSpaProtocols.Register1.ajax).mergeFailure
    val sspRegister2      = ajax.invoker(PublicSpaProtocols.Register2.ajax).mergeFailure

    val awLandingPage = AsyncFeature.Write.D0.init($.zoomStateL(State.landingPage ^|-> LandingPage.State.async))
    val awLogin       = AsyncFeature.Write.D0.init($.zoomStateL(State.login       ^|-> Login      .State.async))
    val awRegister1   = AsyncFeature.Write.D0.init($.zoomStateL(State.register1   ^|-> Register1  .State.async))

    def render(p: Props, s: State): VdomElement = {
      State.recorder.record(s)

      def loginPage(redirectOnLogin: Option[Url.Relative]): VdomElement = {
        val ss = StateSnapshot.zoomL(State.login)(s).setStateVia($)
        Login.Props(ss, awLogin, sspLogin, sspResetPassword1, redirectOnLogin).render
      }

      val content: VdomElement =
        p.page match {

          case Page.Static(PublicSpaRoute.Home) =>
            val ss = StateSnapshot.zoomL(State.landingPage)(s).setStateVia($)
            LandingPage.Props(ss, awLandingPage, p.am, sspLandingPage).render

          case Page.Static(PublicSpaRoute.Login) =>
            loginPage(None)

          case Page.LoginFrom(url) =>
            loginPage(Some(url))

          case Page.Token(PublicSpaRoute.ResetPassword, token) =>
            ResetPassword.Props(token, sspResetPassword2).render

          case Page.Static(PublicSpaRoute.Register1) =>
            val ss = StateSnapshot.zoomL(State.register1)(s).setStateVia($)
            Register1.Props(initData.publicRegistration, p.routerCtl, ss, awRegister1, sspRegister1).render

          case Page.Token(PublicSpaRoute.Register2, token) =>
            Register2.Props(token, sspRegister2).render

          case Page.Static(PublicSpaRoute.Privacy) =>
            Legal.Privacy(p.routerCtl)

          case Page.Static(PublicSpaRoute.TermsOfService) =>
            Legal.TermsOfService(p.routerCtl)
        }

      Layout.Component(Layout.Props(initData.loggedInUser, p.page, p.am, p.routerCtl, content))
    }
  }

}