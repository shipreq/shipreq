package shipreq.webapp.client.public.pages

import japgolly.scalajs.react.test._
import org.scalajs.dom.html
import scalaz.\/-
import utest._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.ajax.TestAjaxClient
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.public._
import shipreq.webapp.client.public.spa._
import PublicSpaTestUtil.semanticUiDisabled

object Register1Tester {

  val * = Dsl[TestAjaxClient, Obs, Unit]

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final class Obs($: DomZipperJs, ajax: TestAjaxClient) {
    val reqsSent = ajax.reqs

    val form: Option[FormObs] =
      $.collect01(".ui.form").doms.map(_ => new FormObs($))

    val message: Option[String] =
      $.collect01(".ui.message .header").innerTexts
  }

  final class FormObs($: DomZipperJs) {
    val emailInput   : html.Input  = $("input[type=email]").domAs[html.Input]
    val emailValue   : String      = emailInput.value
    val emailEnabled : Enabled     = Disabled.when(emailInput.disabled || semanticUiDisabled(emailInput))
    val submit       : html.Button = $("button").domAs[html.Button]
    val submitEnabled: Enabled     = Disabled.when(submit.disabled)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val reqsSent      = *.focus("Requests sent").value(_.obs.reqsSent.length)
  val emailValue    = *.focus("Email text")   .value(_.obs.form.get.emailValue)
  val emailEnabled  = *.focus("Email")        .value(_.obs.form.get.emailEnabled)
  val submitEnabled = *.focus("Submit button").value(_.obs.form.get.submitEnabled)
  val message       = *.focus("Message")      .value(_.obs.message)

  def enterEmail(s: String): *.Actions =
    *.action(s"Set email [$s]")(SimEvent.Change(s) simulate _.obs.form.get.emailInput)

  def clickSubmit: *.Actions =
    *.action("Click submit")(Simulate click _.obs.form.get.submit)
      .addCheck(submitEnabled.assert(Enabled).before)

  def serverResponse: *.Actions =
    *.action("Server response")(_.ref.respondToLast(PublicSpaProtocols.Register1.ajax)(\/-(()))) <+ reqsSent.assert.not.equal(0)
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object Register1Test extends TestSuite {
  import Register1Tester._

  val invariants: *.Invariants =
    *.focus("Form or message, not both").value(x => List(x.obs.form.isDefined, x.obs.message.isDefined).sorted)
      .assert(List(false, true))

  def test(actions: *.Actions, publicRegistration: Permission = Allow): Unit =
    testPlan(Plan(actions, invariants), publicRegistration)

  def testPlan(plan: *.Plan, publicRegistration: Permission = Allow): Unit = {
    val t = new PublicSpaTestUtil.ForTestState
    t.initData = t.initData.copy(publicRegistration = publicRegistration)
    import t.ajax
    t(Page.Register1)(h => plan.test(Observer.watch(new Obs(h, ajax))).stateless.withRef(ajax).run())
  }

  def success: *.Actions = (
    clickSubmit
      +> emailEnabled.assert(Disabled)
      +> submitEnabled.assert(Disabled)
      +> reqsSent.assert.increment
      >> serverResponse
      +> message.assert(Some("Check your email")))

  override def tests = Tests {

    "success" - test(
      emailValue.assert("")        +> emailEnabled.assert(Enabled) +> submitEnabled.assert(Disabled)
        +> enterEmail("x@qwe.com") +> emailEnabled.assert(Enabled) +> submitEnabled.assert(Enabled)
        >> enterEmail("x@")        +> emailEnabled.assert(Enabled) +> submitEnabled.assert(Disabled)
        >> enterEmail("x@qwe.com") +> emailEnabled.assert(Enabled) +> submitEnabled.assert(Enabled)
        >> success)

    "disabled" - test(
      message.assert(Some("Registration disabled")) +> *.emptyAction,
      Deny)

  }
}
