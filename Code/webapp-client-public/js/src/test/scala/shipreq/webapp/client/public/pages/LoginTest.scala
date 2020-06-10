package shipreq.webapp.client.public.pages

import japgolly.scalajs.react.test._
import org.scalajs.dom.{html, window}
import utest._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.ajax._
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.base.user.Username
import shipreq.webapp.client.public._
import shipreq.webapp.client.public.spa._

object LoginTester {
  import PublicSpaTestUtil.semanticUiDisabled

  val * = Dsl[TestAjaxClient, Obs, Unit]

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  final class Obs(val $: DomZipperJs, ajax: TestAjaxClient) {
    val reqsSent = ajax.reqs

    val form: Option[FormObs] =
      $.collect01(".ui.form").doms.map(_ => new FormObs($))
  }

  final class FormObs($: DomZipperJs) {
    val usernameInput    = $("input[type=text]").domAs[html.Input]
    val passwordInput    = $("input[type=password]").domAs[html.Input]
    val rememberMeInput  = $("input[type=checkbox]").domAs[html.Input]
    val username         = usernameInput.value
    val password         = passwordInput.value
    val rememberMe       = On when rememberMeInput.checked
    val login            = $("button").domAs[html.Button]
    val loginEnabled     = Disabled.when(login.disabled || semanticUiDisabled(login))
    val error            = $.collect01(".ui.message.error").zippers
    val errorTitle       = error.map(_(".header").innerText)
    val forgotPwd        = $(".ui.form a").domAs[html.Anchor]
    val forgotPwdEnabled = Disabled.when(semanticUiDisabled(forgotPwd))
  }


  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val reqsSent         = *.focus("Requests sent").value(_.obs.reqsSent.length)
  val username         = *.focus("Username field").value(_.obs.form.get.username)
  val password         = *.focus("Password field").value(_.obs.form.get.password)
  val rememberMe       = *.focus("RememberMe").value(_.obs.form.get.rememberMe)
  val loginEnabled     = *.focus("Login button").value(_.obs.form.get.loginEnabled)
  val forgotPwdEnabled = *.focus("Forgot password").value(_.obs.form.get.forgotPwdEnabled)
  val errorTitle       = *.focus("Error title").value(_.obs.form.get.errorTitle)
  val formVisibility   = *.focus("Login form visibility").value(_.obs.form.isDefined)

  def assertLoginFailed     = errorTitle.assert(Some("Login failed"))
  def assertForgotPwdFailed = errorTitle.assert(Some("Forgotten password"))

  def assertForm(u: String, p: String, r: On): *.Points =
    username.assert(u) & password.assert(p) & rememberMe.assert(r)

  def enterUser(s: String): *.Actions =
    *.action(s"Set username to [$s]")(SimEvent.Change(s) simulate _.obs.form.get.usernameInput)

  def enterPassword(s: String): *.Actions =
    *.action(s"Set password to [$s]")(SimEvent.Change(s) simulate _.obs.form.get.passwordInput)

  def toggleRememberMe: *.Actions =
    *.action("Click remember-me")(Simulate change _.obs.form.get.rememberMeInput)
      .addCheck(rememberMe.assert.changeTo(!_))

  def clickLogin: *.Actions =
    *.action("Click login")(Simulate click _.obs.form.get.login)
      .addCheck(loginEnabled.assert(Enabled).before)

  def serverLoginResponse(p: Permission): *.Actions =
    *.action(s"Server responds to login: $p")(_.ref.respondToLast(CommonProtocols.Login.ajax)(p)) <+ reqsSent.assert.not.equal(0)

  def clickForgotPwd: *.Actions =
    *.action("Click 'Forgot password'")(Simulate click _.obs.form.get.forgotPwd)
      .addCheck(forgotPwdEnabled.assert(Enabled).before)

  def serverForgotPwdResponse: *.Actions =
    *.action("Server responds to forgot-pwd")(_.ref.respondToLast(PublicSpaProtocols.ResetPassword1.ajax)(())) <+ reqsSent.assert.not.equal(0)
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object LoginTest extends TestSuite {
  import LoginTester._

  val invariants: *.Invariants =
    *.emptyInvariant

  def test(actions: *.Actions, loggedInUser: Boolean = false): Unit =
    testPlan(Plan(actions, invariants), loggedInUser)

  def testPlan(plan: *.Plan, loggedInUser: Boolean = false): Unit = {
    val t = new PublicSpaTestUtil.ForTestState
    if (loggedInUser)
      t.initData = t.initData.copy(loggedInUser = Some(Username("dude")))
    import t.ajax
    t(Page.Login)(h => plan.test(Observer.watch(new Obs(h, ajax))).stateless.withRef(ajax).run())
  }

  // Can't test window.location.href because relative URLs are rejected by PhantomJS
  def success: *.Actions = (
    clickLogin
      +> loginEnabled.assert(Disabled)
      +> reqsSent.assert.increment
      >> serverLoginResponse(Allow))

  override def tests = Tests {
    window.localStorage.clear()

    "login" - {
      "localValidation" - test(
        clickLogin
          +> loginEnabled.assert(Enabled)
          +> assertLoginFailed
          +> reqsSent.assert(0))

      "success" - test(enterUser("blah") >> enterPassword("hhhhhh345345") >> success)

      "failureThenSuccess" - test(
        enterUser("blah") >> enterPassword("hhhhhh345345") +> errorTitle.assert(None) >> (
          clickLogin
            +> loginEnabled.assert(Disabled)
            +> reqsSent.assert.increment
            >> serverLoginResponse(Deny)
            +> loginEnabled.assert(Enabled)
            +> assertLoginFailed
            +> reqsSent.assert.noChange
          ).times(2)
          >> success)

      "rememberMe" - {
        test(assertForm("", "", On) +> enterUser("b") >> enterPassword("x"))
        test(assertForm("b", "", On) +> toggleRememberMe +> assertForm("b", "", Off))
        test(assertForm("", "", Off) +> enterUser("b") >> enterPassword("x"))
        test(assertForm("", "", Off) +> enterUser(" q ") >> toggleRememberMe)
        test(assertForm("q", "", On) +> *.emptyAction)
      }

//      'loggedInRedirects {
//      }
    }

    "resetPassword" - {
      "localValidation" - test(
        clickForgotPwd
          +> loginEnabled.assert(Enabled)
          +> assertForgotPwdFailed
          +> reqsSent.assert(0))

      "success" - test(
        enterUser("blah")
          >> clickForgotPwd
            +> loginEnabled.assert(Disabled)
            +> reqsSent.assert.increment
          >> serverForgotPwdResponse
            +> formVisibility.assert(false))
    }
  }
}
