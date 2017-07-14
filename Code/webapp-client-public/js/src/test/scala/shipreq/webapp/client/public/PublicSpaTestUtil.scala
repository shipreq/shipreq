package shipreq.webapp.client.public

import japgolly.scalajs.react.test._
import org.scalajs.dom.html
import scala.annotation.tailrec
import teststate.data.Id
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.ServerSideProc
import shipreq.webapp.base.test.TestClientProtocol
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.public.spa.{Page, PublicSpa}

object PublicSpaTestUtil {

  val initData = PublicSpaProtocols.InitData(
    allowRegister  = Allow,
    loggedInUser   = None,
    landingPage    = ServerSideProc("landingPage"   , PublicSpaProtocols.LandingPage.Fn),
    register1      = ServerSideProc("register1"     , PublicSpaProtocols.Register.Fn1),
    register2      = ServerSideProc("register2"     , PublicSpaProtocols.Register.Fn2),
    login          = ServerSideProc("login"         , PublicSpaProtocols.Login.Fn),
    resetPassword1 = ServerSideProc("resetPassword1", PublicSpaProtocols.ResetPassword.Fn1),
    resetPassword2 = ServerSideProc("resetPassword2", PublicSpaProtocols.ResetPassword.Fn2))

  class ForTestState {
    val cp       = new TestClientProtocol(false)
    val rc       = MockRouterCtl[Page]()
    var initData = PublicSpaTestUtil.initData

    def render[A](initPage: Page)(f: HtmlDomZipper => A): A = {
      val spa = new PublicSpa(initData, cp)
      ReactTestUtils.withRenderedIntoDocument(spa.Component(PublicSpa.Props(initPage, rc))) { m =>
        f(m.htmlDomZipper)
      }
    }

    def apply(initPage: Page)(f: HtmlDomZipper => Report[String]): Unit =
      render(initPage) { h =>
        val r = f(h)
        assertTestState(r)
        ()
      }
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  abstract class InputFieldObs($: HtmlDomZipper, val input: html.Input) {
    val failure : Option[String] = $.collect01(">div:not(.ui),>span").innerTexts
    val enabled : Enabled        = Disabled.when(input.disabled || semanticUiDisabled($.dom))
    val validity: Validity       = Invalid when $.domAsHtml.classList.contains("error")
  }

  class TextFieldObs($: HtmlDomZipper) extends InputFieldObs($,
    $("input[type=text],input[type=email],input[type=password],textarea").domAs[html.Input]) {
    val value: String = input.value
  }

  class CheckboxFieldObs($: HtmlDomZipper) extends InputFieldObs($,
    $("input[type=checkbox]").domAs[html.Input]) {
    val checked: Boolean = input.checked
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  class InputFieldDsl[R, O, S](final val * : Dsl[Id, R, O, S, String], desc: String, f: O => InputFieldObs) {
    val enabled    = *.focus(desc).value($ => f($.obs).enabled)
    val validity   = *.focus(desc).value($ => f($.obs).validity)
    val failure    = *.focus(s"$desc failure").value($ => f($.obs).failure)
    val hasFailure = failure.map(_.isDefined)
  }

  class TextFieldDsl[R, O, S](dsl: Dsl[Id, R, O, S, String])(desc: String, f: O => TextFieldObs) extends InputFieldDsl(dsl, desc, f) {
    val text = *.focus(desc).value($ => f($.obs).value)
    val tv   = *.focus(s"$desc text & validity").value($ => (f($.obs).value, f($.obs).validity))

    def set(text: String): *.Actions =
      *.action(s"Set $desc to [$text]")($ => SimEvent.Change(text) simulate f($.obs).input) <+ enabled.assert(Enabled)
  }

  class CheckboxFieldDsl[R, O, S](dsl: Dsl[Id, R, O, S, String])(desc: String, f: O => CheckboxFieldObs) extends InputFieldDsl(dsl, desc, f) {
    val checked = *.focus(s"$desc.checked").value($ => f($.obs).checked)

    lazy val click: *.Actions =
      *.action(s"Click $desc")($ => Simulate change f($.obs).input) <+ enabled.assert(Enabled)

    lazy val check: *.Actions =
      click.unless($ => f($.obs).checked).rename(s"Check $desc")

    lazy val uncheck: *.Actions =
      click.when($ => f($.obs).checked).rename(s"Uncheck $desc")
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  @tailrec
  def semanticUiDisabled(h: html.Element): Boolean =
    if (h.classList.contains("disabled"))
      true
    else {
      val n = h.parentElement
      if (n eq null)
        false
      else
        semanticUiDisabled(n)
    }
}
