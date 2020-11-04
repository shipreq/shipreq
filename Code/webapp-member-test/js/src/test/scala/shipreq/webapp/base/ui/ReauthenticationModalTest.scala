package shipreq.webapp.base.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.test._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.{Element, document, html}
import shipreq.base.util._
import shipreq.webapp.base.data.Username
import shipreq.webapp.base.test.TestReauthenticationModal
import shipreq.webapp.base.test.TestState._
import utest._

object ReauthenticationModalTest extends TestSuite {

  private object Internal {

    val username = Username("aiden")

    final case class Props(login: ReauthenticationModal.AttemptLogin, root: Element)

    final class Backend($: BackendScope[Props, Unit]) {
      var modal: ReauthenticationModal = null

      def render(props: Props): VdomElement = {
        modal = ReauthenticationModal(username, props.login, props.root, 0)
        <.div(modal.render)
      }

      val onMount =
        CallbackTo(modal).asAsyncCallback.flatMap(_.run).toCallback

      def dom() = $.getDOMNode.runNow().toHtml.get
    }

    val Component = ScalaComponent.builder[Props]
      .renderBackend[Backend]
      .componentDidMount(_.backend.onMount)
      .build

    // =================================================================================================================

    final case class Ref(backend: Backend, server: TestReauthenticationModal)

    val * = Dsl[Ref, Obs, Unit]

    def runTest(initialResponse: Option[ErrorMsg \/ Permission])(a: *.Actions) = {
      val a2 = a <+ passwordValue.assert("")
      val server = TestReauthenticationModal(initialResponse)
      ReactTestUtils.withNewDocumentElement { root =>
        val m = Component(Props(server.proc, root)).renderIntoDOM(root)
        Plan.action(a2)
          .test(observer)
          .stateless
          .withRef(Ref(m.backend, server))
          .run()
          .assert()
      }
    }

    // =================================================================================================================

    final class Obs($: DomZipperJs, server: TestReauthenticationModal) {
      import Obs._

      private val password = $("input[type=password]")
      def passwordDom()  = password.domAs[html.Input]
      val passwordValue  = password.value
      val passwordEnabled = !passwordDom().readOnly

      def setPassword(p: String) = {
        val dom = passwordDom()
        dom.value = p
        SimEvent.Change(p).simulate(dom)
      }

      val cancelButton = new Button($("button", 1 of 2))
      val loginButton  = new Button($("button", 2 of 2))

      private val errorMsgDom = $(".label").domAs[html.Div]
      val errorMsgVisible = errorMsgDom.style.display != "none"
      val errorMsg        = Option.when(errorMsgVisible)(errorMsgDom.textContent.trim)

      val serverAttempts = server.attempts
    }

    object Obs {
      final class Button($: DomZipperJs) {
        val dom      = $.domAs[html.Button]
        val disabled = dom.disabled
        def enabled  = !disabled
        def click()  = Simulate.click(dom)
      }
    }

    val observer: Observer[Ref, Obs, String] =
      Observer { ref =>
        val $: DomZipperJs =
          DomZipperJs(ref.backend.dom()).collect01(".modal").zippers.getOrElse {
            DomZipperJs(document.body)
              .collect0n(".modal")
              .filter(_(".header").innerText == ReauthenticationModal.header)
              .singleton
          }
        new Obs($, ref.server)
      }

    // =================================================================================================================

    val errorMsg           = *.focus("error message").option(_.obs.errorMsg)
    val loginButtonEnabled = *.focus("login button enabled").value(_.obs.loginButton.enabled)
    val passwordEnabled    = *.focus("password enabled").value(_.obs.passwordEnabled)
    val passwordValue      = *.focus("password").value(_.obs.passwordValue)
    val serverCallCount    = *.focus("server call count").value(_.obs.serverAttempts.length)
    val serverLastPassword = *.focus("most recently submitted password").option(_.obs.serverAttempts.lastOption.map(_.password.value))

    val clickCancel = *.action("Click cancel")(_.obs.cancelButton.click())
    val clickLogin  = *.action("Click login")(_.obs.loginButton.click())

    def enterPassword(p: String) =
      *.action(s"Enter password: '$p'")(_.obs.setPassword(p)) +> passwordValue.assert(p)

    def setNextServerResponse(r: Option[ErrorMsg \/ Permission]) =
      *.action(s"Set next server response to $r")(_.ref.server.nextResponse = r)
  }

  // ===================================================================================================================

  // NOTE: This shit is really hard to test because of damn Semantic UI and it's JQuery bullshit.
  //
  // 1. Can't tell if a modal is visible or not
  // 2. Transitions won't run meaning the onHiding callback doesn't get executed

  private val invalidPassword = "Invalid password."

  override def tests = Tests {
    import Internal._

    "various" -
      runTest(Some(-\/(ErrorMsg("servers down!"))))(
        enterPassword(" omg ")
          +> serverCallCount.assert(0)
          +> errorMsg.assert(None)
          +> loginButtonEnabled.assert(true)
          +> passwordEnabled.assert(true)

          >> clickLogin
          +> serverCallCount.assert(0) // validation should prevent this call
          +> errorMsg.assert(Some(invalidPassword))
          +> loginButtonEnabled.assert(true)
          +> passwordEnabled.assert(true)

          >> enterPassword(" omgOMFG12345 ")
          +> serverCallCount.assert(0)
          +> errorMsg.assert(None)
          +> loginButtonEnabled.assert(true)
          +> passwordEnabled.assert(true)

          >> clickLogin
          +> serverCallCount.assert(1)
          +> errorMsg.assert(Some("servers down!"))
          +> loginButtonEnabled.assert(true)
          +> passwordEnabled.assert(true)

          >> setNextServerResponse(Some(\/-(Deny)))
          >> clickLogin
          +> serverCallCount.assert(2)
          +> errorMsg.assert(Some(invalidPassword))
          +> loginButtonEnabled.assert(true)
          +> passwordEnabled.assert(true)

          >> setNextServerResponse(Some(\/-(Allow)))
          >> clickLogin
          +> serverCallCount.assert(3)
          +> errorMsg.assert(None)
          +> loginButtonEnabled.assert(false) // login succeeds so modal *would* close triggering a reset on completion
          +> serverLastPassword.assert(Some(" omgOMFG12345 "))
      )

    "inFlight" -
      runTest(None)(
        enterPassword("qweasdzcQWEASDZXC123!@#")
          >> clickLogin
          +> serverCallCount.assert(1)
          +> serverLastPassword.assert(Some("qweasdzcQWEASDZXC123!@#"))
          +> loginButtonEnabled.assert(false)
          +> passwordEnabled.assert(false)
          +> errorMsg.assert(None)
      )

  }
}
