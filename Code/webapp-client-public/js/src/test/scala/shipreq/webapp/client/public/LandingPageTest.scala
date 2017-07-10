package shipreq.webapp.client.public

import japgolly.scalajs.react.test._
import org.scalajs.dom.html
import utest._
import shipreq.base.util._
import shipreq.webapp.base.data.{Disabled, Enabled}
import shipreq.webapp.base.protocol.ServerSideProc
import shipreq.webapp.base.test._
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.public.spa._

object LandingPageTester {

  val * = Dsl[TestClientProtocol, Obs, Unit]

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  class TextFieldObs($: HtmlDomZipper) {
    val input              = $("input,textarea").forceDomAs[html.Input]
    val text    : String   = input.value
    val enabled : Enabled  = Disabled.when(input.disabled || $.dom.classList.contains("disabled"))
    val validity: Validity = Invalid when $.dom.classList.contains("error")
  }

  final class Obs($: HtmlDomZipper, cp: TestClientProtocol) {
    val reqsSent = cp.reqs

    private val form              = $(".ui.form")
    private def field(i: Int)     = form(">.field", i of 5).asHtml
    private def textField(i: Int) = new TextFieldObs(field(i))

    val name  = textField(1)
    val email = textField(2)

    val submitField                = field(5)
    val submit       : html.Button = form("button").domAs[html.Button]
    val submitEnabled: Enabled     = Disabled.when(submit.disabled || submitField.dom.classList.contains("disabled"))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final class TextFieldDsl(desc: String, f: Obs => TextFieldObs) {
    val text     = *.focus(s"$desc text"    ).value($ => f($.obs).text)
    val enabled  = *.focus(s"$desc enabled" ).value($ => f($.obs).enabled)
    val validity = *.focus(s"$desc validity").value($ => f($.obs).validity)
    val state    = *.focus(s"$desc text & validity").value($ => (f($.obs).text, f($.obs).validity))

    def set(text: String): *.Actions =
      *.action(s"Set $desc to [$text]")($ => SimEvent.Change(text) simulate f($.obs).input)
  }

  val reqsSent      = *.focus("Requests sent").value(_.obs.reqsSent.length)
  val name          = new TextFieldDsl("Name", _.name)
  val email         = new TextFieldDsl("Email", _.email)
  val submitEnabled = *.focus("Submit button enabled").value(_.obs.submitEnabled)

  def initialState: *.Points =
    name.state.assert("", Valid) & email.state.assert("", Valid)

  def clickSubmit: *.Actions =
    *.action("Click submit")(Simulate click _.obs.submit)
      .addCheck(submitEnabled.assert(Enabled).before)

  def enterValidDetails: *.Actions = (
    name.set("Bob") +> name.state.assert("Bob", Valid)
      >> email.set("bob@qwe.com") +> email.state.assert("bob@qwe.com", Valid))

  def submitSuccessfully: *.Actions = (
    clickSubmit
      +> reqsSent.assert.increment
      +> formDisabled // async InProgress
      >> serverRespondOk
      +> formDisabled) // submitted

  def serverRespondOk: *.Actions =
    *.action("Server responds indicating success")(_.ref.autoRespondToLast()) <+ reqsSent.assert.not.equal(0)

  def formDisabled: *.Points =
    name.enabled.assert(Disabled) &
    email.enabled.assert(Disabled) &
    submitEnabled.assert(Disabled)
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object LandingPageTest extends TestSuite {
  import LandingPageTester._

  val invariants: *.Invariants =
    submitEnabled.assert(Disabled).when(i => i.obs.name.validity.is(Invalid) || i.obs.email.validity.is(Invalid))

  def validity: *.Actions =
    initialState +> enterValidDetails >> submitSuccessfully

  def invalidity: *.Actions = (
    initialState
      +> clickSubmit +> submitEnabled.assert(Disabled) +> name.state.assert("", Invalid) +> email.state.assert("", Invalid)
      >> email.set("bob") +> email.state.assert("bob", Invalid)
      >> enterValidDetails +> reqsSent.assert(0)
      >> submitSuccessfully)

  def test(actions: *.Actions): Unit =
    test(Plan(actions, invariants))

  def test(plan: *.Plan): Unit = {
    val cp = new TestClientProtocol(false)
    cp.addAutoResponse(PublicSpaProtocols.LandingPage.Fn)(_.success(()))

    val i = PublicSpaProtocols.InitData(
      landingPage     = ServerSideProc("landingPage    ", PublicSpaProtocols.LandingPage.Fn),
      allowRegister   = Allow,
      register1       = ServerSideProc("register1      ", PublicSpaProtocols.Register.Fn1),
      register2A      = ServerSideProc("register2A     ", PublicSpaProtocols.Register.Fn2A),
      register2B      = ServerSideProc("register2B     ", PublicSpaProtocols.Register.Fn2B),
      login           = ServerSideProc("login          ", PublicSpaProtocols.Login.Fn),
      resetPassword1  = ServerSideProc("resetPassword1 ", PublicSpaProtocols.ResetPassword.Fn1),
      resetPassword2A = ServerSideProc("resetPassword2A", PublicSpaProtocols.ResetPassword.Fn2A),
      resetPassword2B = ServerSideProc("resetPassword2B", PublicSpaProtocols.ResetPassword.Fn2B))

    val spa   = new PublicSpa(i, cp)
    val rc    = MockRouterCtl[Page]()
    val props = PublicSpa.Props(Page.Home, rc)

    ReactTestUtils.withRenderedIntoBody(spa.Component(props)) { m =>
      val t = plan.test(Observer.watch(new Obs(m.htmlDomZipper, cp)))
      val r = t.run((), cp)
      assertTestState(r)
    }
  }

  override def tests = TestSuite {
    'validity - test(validity)
    'invalidity - test(invalidity)
  }
}
