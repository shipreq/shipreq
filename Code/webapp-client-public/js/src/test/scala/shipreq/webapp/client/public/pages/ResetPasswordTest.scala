package shipreq.webapp.client.public.pages

import japgolly.scalajs.react.test._
import org.scalajs.dom.html
import scalaz.\/-
import utest._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.test._
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.client.public._
import shipreq.webapp.client.public.spa._
import shipreq.webapp.base.Urls
import PublicSpaProtocols.ResetPassword2.Result

object ResetPasswordTester {

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
    val password1     = new PublicSpaTestUtil.TextFieldObs($(".field", 1 of 2))
    val password2     = new PublicSpaTestUtil.TextFieldObs($(".field", 2 of 2))
    val submit        = $("button").domAs[html.Button]
    val submitEnabled = Disabled.when(submit.disabled)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val reqsSent      = *.focus("Requests sent").value(_.obs.reqsSent.length)
  val password1T    = *.focus("Password 1").value(_.obs.form.get.password1.value)
  val password2T    = *.focus("Password 2").value(_.obs.form.get.password2.value)
  val password1E    = *.focus("Password 1").value(_.obs.form.get.password1.enabled)
  val password2E    = *.focus("Password 2").value(_.obs.form.get.password2.enabled)
  val password1V    = *.focus("Password 1").value(_.obs.form.get.password1.validity)
  val password2V    = *.focus("Password 2").value(_.obs.form.get.password2.validity)
  val password1F    = *.focus("Password 1 error msg").value(_.obs.form.get.password1.failure)
  val password2F    = *.focus("Password 2 error msg").value(_.obs.form.get.password2.failure)
  val submitEnabled = *.focus("Submit button").value(_.obs.form.get.submitEnabled)
  val message       = *.focus("Message").value(_.obs.message)

  def assertForm(p1: String, v1: Validity,
                 p2: String, v2: Validity, e: Enabled): *.Points =
    password1T.assert(p1) & password1V.assert(v1) &
    password2T.assert(p2) & password2V.assert(v2) & assertForm(e)

  def assertForm(e: Enabled): *.Points =
    password1E.assert(e) & password2E.assert(e)

  def enterPassword1(s: String): *.Actions =
    *.action(s"Set password 1 to [$s]")(SimEvent.Change(s) simulate _.obs.form.get.password1.input)

  def enterPassword2(s: String): *.Actions =
    *.action(s"Set password 2 to [$s]")(SimEvent.Change(s) simulate _.obs.form.get.password2.input)

  def clickSubmit: *.Actions =
    *.action("Click submit")(Simulate click _.obs.form.get.submit)
      .addCheck(submitEnabled.assert(Enabled).before)

  def serverResponse(r: Result): *.Actions =
    *.action(s"Server response: $r")(_.ref.respondToLast(PublicSpaProtocols.ResetPassword2.ajax)(\/-(r))) <+ reqsSent.assert.not.equal(0)
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object ResetPasswordTest extends TestSuite {
  import ResetPasswordTester._

  val invariants: *.Invariants =
    *.focus("Form or message, not both").value(x => List(x.obs.form.isDefined, x.obs.message.isDefined).sorted)
      .assert(List(false, true))

  def test(actions: *.Actions): Unit =
    test(Plan(actions, invariants))

  def test(plan: *.Plan): Unit = {
    val t = new PublicSpaTestUtil.ForTestState
    import t.ajax
    t(page)(h => plan.test(Observer.watch(new Obs(h, ajax))).stateless.withRef(ajax).run())
  }

  val page = Page.Token(Urls.PublicSpaRoute.ResetPassword, VerificationToken("abcd1234"))

  def success: *.Actions = (
    clickSubmit
      +> assertForm(Disabled)
      +> submitEnabled.assert(Disabled)
      +> reqsSent.assert.increment
      >> serverResponse(Result.Success)
      +> message.assert(Some("Changed Password")))

  val p1 = "abcdEFGH1234"
  val p2 = "zyx_abcdEFGH123456"

  override def tests = Tests {

    'success - test(
      assertForm("", Invalid, "", Invalid, Enabled)
        +> submitEnabled.assert(Disabled)
        +> password1F.assert(Some("Must be between 8 and 255 characters long."))
        +> password2F.assert(None)

        +> enterPassword1("x" * 8) +> assertForm("x" * 8, Invalid, "", Invalid, Enabled) +> submitEnabled.assert(Disabled)
        +> password1F.assert(Some("Must contain at least one letter, and at least one number."))
        +> password2F.assert(None)

        >> enterPassword1(p1)
        +> assertForm(p1, Valid, "", Invalid, Enabled)
        +> submitEnabled.assert(Disabled)

        >> enterPassword2(p2)
        +> assertForm(p1, Valid, p2, Invalid, Enabled)
        +> submitEnabled.assert(Disabled)
        +> password2F.assert(None)

        >> enterPassword1(p2)
        +> assertForm(p2, Valid, p2, Valid, Enabled)
        +> submitEnabled.assert(Enabled)

        >> success)

  }
}
