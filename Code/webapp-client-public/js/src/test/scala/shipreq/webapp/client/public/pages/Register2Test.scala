package shipreq.webapp.client.public.pages

import japgolly.scalajs.react.test._
import org.scalajs.dom.html
import shipreq.base.util._
import shipreq.webapp.base.Urls
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.ajax.TestAjaxClient
import shipreq.webapp.base.test.TestState.{Result => _, _}
import shipreq.webapp.client.public.PublicSpaProtocols
import shipreq.webapp.client.public.PublicSpaProtocols.Register2._
import shipreq.webapp.client.public.PublicSpaTestUtil._
import shipreq.webapp.client.public.spa._
import utest._

object Register2Tester {

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
    private def field(i: Int)           = $(".field", i of 7)
    val name         : TextFieldObs     = new TextFieldObs(field(1))
    val username     : TextFieldObs     = new TextFieldObs(field(2))
    val password1    : TextFieldObs     = new TextFieldObs(field(3))
    val password2    : TextFieldObs     = new TextFieldObs(field(4))
    val newsletter   : CheckboxFieldObs = new CheckboxFieldObs(field(5))
    val tos          : CheckboxFieldObs = new CheckboxFieldObs(field(6))
    val submit       : html.Button      = $("button").domAs[html.Button]
    val submitEnabled: Enabled          = Disabled.when(submit.disabled)

    val textFields     = name :: username :: password1 :: password2 :: Nil
    val checkboxFields = newsletter :: tos :: Nil
    val fields         = textFields ::: checkboxFields
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  val reqsSent      = *.focus("Requests sent").value(_.obs.reqsSent.length)
  val name          = new TextFieldDsl(*)("Name",      _.form.get.name)
  val username      = new TextFieldDsl(*)("Username",  _.form.get.username)
  val password1     = new TextFieldDsl(*)("Password1", _.form.get.password1)
  val password2     = new TextFieldDsl(*)("Password2", _.form.get.password2)
  val newsletter    = new CheckboxFieldDsl(*)("Newsletter", _.form.get.newsletter)
  val tos           = new CheckboxFieldDsl(*)("TermsOfService", _.form.get.tos)
  val submitEnabled = *.focus("Submit button").value(_.obs.form.get.submitEnabled)
  val message       = *.focus("Message")      .value(_.obs.message)

  val textFields     = name :: username :: password1 :: password2 :: Nil
  val checkboxFields = newsletter :: tos :: Nil
  val fields         = textFields ::: checkboxFields

  def assertForm(e: Enabled): *.Points =
    fields.map(_.enabled.assert(e)).reduce(_ & _) & submitEnabled.assert(e)

  def clickSubmit: *.Actions =
    *.action("Click submit")(Simulate click _.obs.form.get.submit)
      .addCheck(submitEnabled.assert(Enabled).before)

  def queriesServer: *.Arounds =
    assertForm(Disabled).after &
      submitEnabled.assert(Disabled).after &
      reqsSent.assert.increment

  def serverResponse(r: Result): *.Actions =
    *.action(s"Server responds with $r")(_.ref.respondToLast(PublicSpaProtocols.Register2.ajax)(\/-(r))) <+ reqsSent.assert.not.equal(0)

  private val pwd = "qqqqqq123QWE"

  def enterValidDetails: *.Actions = (
    name.set("Ol' Bob") +> name.tv.assert(("Ol' Bob", Valid))
      >> username.set("bob100") +> username.tv.assert(("bob100", Valid))
      >> password1.set(pwd) +> password1.tv.assert((pwd, Valid))
      >> password2.set(pwd) +> password2.tv.assert((pwd, Valid))
      >> tos.check +> tos.validity.assert(Valid))
    .group("Enter valid details")

  def respondUsernameTaken: *.Actions = (
    serverResponse(Result.UsernameTaken)
      +> username.validity.assert(Invalid)
      +> username.failure.assert(Some("Already in use.")))

  val submitToUsernameTaken: *.Actions =
    clickSubmit +> queriesServer >> respondUsernameTaken
}

// █████████████████████████████████████████████████████████████████████████████████████████████████████████████████████

object Register2Test extends TestSuite {
  import Register2Tester._

  val invariants: *.Invariants = {

    val formOrMsg =
      *.focus("").value(x => List(x.obs.form.isDefined, x.obs.message.isDefined).sorted).assert(List(false, true))
        .rename("Form or message, not both")

    val invalidFieldsDisableSubmit =
      submitEnabled.assert(Disabled)
        .when(_.obs.form.exists(_.fields.exists(_.validity.is(Invalid))))

    val submitMeansAllFieldsValid =
      fields.map(_.validity.assert(Valid)).reduce(_ & _)
        .when(_.obs.form.exists(_.submitEnabled is Enabled))

    formOrMsg & invalidFieldsDisableSubmit & submitMeansAllFieldsValid
  }

  def test(actions: *.Actions): Unit =
    test(Plan(actions, invariants))

  def test(plan: *.Plan): Unit = {
    val t = new ForTestState
    import t.ajax
    t(page)(h => plan.test(Observer.watch(new Obs(h, ajax))).stateless.withRef(ajax).run())
  }

  val page = Page.Token(Urls.PublicSpaRoute.Register2, VerificationToken("abcd1234"))

  def success: *.Actions = (
    clickSubmit
      +> queriesServer
      >> serverResponse(Result.Success)
      +> message.assert(Some("Welcome to ShipReq!")))

  override def tests = Tests {

    "success" - test(
      assertForm(Enabled)
        +> tos.checked.assert(false)
        +> enterValidDetails
        >> success)

    "failureThenSuccess" - test(
      enterValidDetails
        >> username.set("")       +> submitEnabled.assert(Enabled)
        >> clickSubmit            +> username.validity.assert(Invalid) +> reqsSent.assert.noChange
        >> username.set("okfine") +> submitEnabled.assert(Enabled)
        >> username.set("")       +> username.validity.assert(Invalid)
        >> username.set("okfine") +> submitEnabled.assert(Enabled)
        >> success)

    "remembersTakenUsernames" - test(
      enterValidDetails
        >> username.set("hello") +> submitEnabled.assert(Enabled)
        >> submitToUsernameTaken
        >> username.set("name2") +> submitEnabled.assert(Enabled)
        >> submitToUsernameTaken
        >> username.set("great") +> submitEnabled.assert(Enabled)
        >> username.set("hello") +> username.validity.assert(Invalid)
        >> username.set("great") +> submitEnabled.assert(Enabled)
        >> username.set(" name2 ") +> username.validity.assert(Invalid)
        >> username.set("great") +> submitEnabled.assert(Enabled)
        >> success)

    "tosRequired" - test(
      enterValidDetails
        >> tos.uncheck
        >> clickSubmit +> tos.validity.assert(Invalid) +> reqsSent.assert.noChange
        >> tos.check   +> submitEnabled.assert(Enabled)
        >> tos.uncheck +> tos.validity.assert(Invalid)
        >> tos.check   +> submitEnabled.assert(Enabled)
        >> success)

  }
}
