package shipreq.webapp.client.public.pages

import japgolly.scalajs.react.test._
import org.scalajs.dom.html
import scalaz.\/-
import shipreq.base.util._
import shipreq.webapp.base.data.{Disabled, Enabled}
import shipreq.webapp.base.protocol.ajax.TestAjaxClient
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.public.PublicSpaTestUtil._
import shipreq.webapp.client.public._
import shipreq.webapp.client.public.spa._
import utest._

object LandingPageTester {

  val * = Dsl[TestAjaxClient, Obs, Unit]

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final class Obs($: DomZipperJs, ajax: TestAjaxClient) {
    val reqsSent = ajax.reqs

    private val form              = $(".ui.form")
    private def field(i: Int)     = form(">.field", i of 5)
    private def textField(i: Int) = new TextFieldObs(field(i))

    val name  = textField(1)
    val email = textField(2)

    val submitField                = field(5)
    val submit       : html.Button = form("button").domAs[html.Button]
    val submitEnabled: Enabled     = Disabled.when(submit.disabled || semanticUiDisabled(submitField.domAsHtml))
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val reqsSent      = *.focus("Requests sent").value(_.obs.reqsSent.length)
  val name          = new TextFieldDsl(*)("Name", _.name)
  val email         = new TextFieldDsl(*)("Email", _.email)
  val submitEnabled = *.focus("Submit button").value(_.obs.submitEnabled)

  def initialState: *.Points =
    name.tv.assert(("", Valid)) & email.tv.assert(("", Valid))

  def clickSubmit: *.Actions =
    *.action("Click submit")(Simulate click _.obs.submit)
      .addCheck(submitEnabled.assert(Enabled).before)

  def enterValidDetails: *.Actions = (
    name.set("Bob") +> name.tv.assert(("Bob", Valid))
      >> email.set("bob@qwe.com") +> email.tv.assert(("bob@qwe.com", Valid)))

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
      +> clickSubmit +> submitEnabled.assert(Disabled) +> name.tv.assert(("", Invalid)) +> email.tv.assert(("", Invalid))
      >> email.set("bob") +> email.tv.assert(("bob", Invalid))
      >> enterValidDetails +> reqsSent.assert(0)
      >> submitSuccessfully)

  def test(actions: *.Actions): Unit =
    test(Plan(actions, invariants))

  def test(plan: *.Plan): Unit = {
    val t = new ForTestState
    import t.ajax
    ajax.addAutoResponse(PublicSpaProtocols.LandingPage.ajax)(_.onResponse(\/-(\/-(\/-(())))))
    t(Page.Home)(h => plan.test(Observer.watch(new Obs(h, ajax))).stateless.withRef(ajax).run())
  }

  override def tests = Tests {
    "validity" - test(validity)
    "invalidity" - test(invalidity)
  }
}
