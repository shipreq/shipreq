package shipreq.webapp.base.ui

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.{Element, document, html}
import japgolly.scalajs.react.test._
import scalaz.{-\/, \/, \/-}
import shipreq.base.util._
import shipreq.webapp.base.test.TestFeedbackModal
import shipreq.webapp.base.test.TestState._
import shipreq.webapp.base.user.Username
import utest._

object FeedbackModalTest extends TestSuite {

  private object Internal {

    val username = Username("aiden")

    final case class Props(submitFn: FeedbackModal.SubmitFn, root: Element)

    final class Backend($: BackendScope[Props, Unit]) {
      var modal: FeedbackModal = null

      def render(props: Props): VdomElement = {
        modal = FeedbackModal(props.submitFn, props.root)
        <.div(modal.render)
      }

      val onMount =
        CallbackTo(modal).asAsyncCallback.flatMap(_.run).toCallback

      def dom() = $.getDOMNode.runNow().toHtml.get
    }

    val Component = ScalaComponent.builder[Props]("")
      .renderBackend[Backend]
      .componentDidMount(_.backend.onMount)
      .build

    // =================================================================================================================

    final case class Ref(backend: Backend, server: TestFeedbackModal)

    val * = Dsl[Ref, Obs, Unit]

    def runTest(initialResponse: Option[ErrorMsg \/ Unit])(a: *.Actions) = {
      val a2 = a <+ feedbackValue.assert("")
      val server = TestFeedbackModal(initialResponse)
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

    final class Obs($: DomZipperJs, server: TestFeedbackModal) {
      import Obs._

      private val feedback = $("textarea")
      def feedbackDom()   = feedback.domAs[html.TextArea]
      val feedbackValue   = feedback.value
      val feedbackEnabled = !feedbackDom().readOnly

      def setFeedback(p: String) = {
        val dom = feedbackDom()
        dom.value = p
        SimEvent.Change(p).simulate(dom)
      }

      val cancelButton = new Button($("button", 1 of 2))
      val sendButton   = new Button($("button", 2 of 2))

      private val errorMsgDom = $(".message").domAs[html.Div]
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
        val $ =
          DomZipperJs(ref.backend.dom()).collect01(".modal").zippers
            .getOrElse(DomZipperJs(document.body).apply(".modal"))
        new Obs($, ref.server)
      }

    // =================================================================================================================

    val errorMsg           = *.focus("error message").option(_.obs.errorMsg)
    val sendButtonEnabled  = *.focus("submit button enabled").value(_.obs.sendButton.enabled)
    val feedbackEnabled    = *.focus("feedback enabled").value(_.obs.feedbackEnabled)
    val feedbackValue      = *.focus("feedback").value(_.obs.feedbackValue)
    val serverCallCount    = *.focus("server call count").value(_.obs.serverAttempts.length)
    val serverLastFeedback = *.focus("most recently submitted feedback").option(_.obs.serverAttempts.lastOption.map(_.feedback.value))

    val clickCancel = *.action("Click cancel")(_.obs.cancelButton.click())
    val clickSend   = *.action("Click submit")(_.obs.sendButton.click())

    def enterFeedback(p: String) =
      *.action(s"Enter feedback: '$p'")(_.obs.setFeedback(p)) +> feedbackValue.assert(p)

    def setNextServerResponse(r: Option[ErrorMsg \/ Unit]) =
      *.action(s"Set next server response to $r")(_.ref.server.nextResponse = r)
  }

  // ===================================================================================================================

  // NOTE: This shit is really hard to test because of damn Semantic UI and it's JQuery bullshit.
  //
  // 1. Can't tell if a modal is visible or not
  // 2. Transitions won't run meaning the onHiding callback doesn't get executed

  override def tests = Tests {
    import Internal._

    'various -
      runTest(Some(-\/(ErrorMsg("servers down!"))))(
        enterFeedback(" \n ")
          +> serverCallCount.assert(0)
          +> errorMsg.assert(None)
          +> sendButtonEnabled.assert(true)
          +> feedbackEnabled.assert(true)

          >> clickSend
          +> serverCallCount.assert(0) // validation should prevent this call
          +> errorMsg.assert(Some(FeedbackModal.errorEmptyFeedback.value))
          +> sendButtonEnabled.assert(true)
          +> feedbackEnabled.assert(true)

          >> enterFeedback(" omgOMFG12345 ")
          +> serverCallCount.assert(0)
          +> errorMsg.assert(None)
          +> sendButtonEnabled.assert(true)
          +> feedbackEnabled.assert(true)

          >> clickSend
          +> serverCallCount.assert(1)
          +> errorMsg.assert(Some("servers down!"))
          +> sendButtonEnabled.assert(true)
          +> feedbackEnabled.assert(true)

          >> setNextServerResponse(Some(-\/(ErrorMsg("Failed to connect"))))
          >> clickSend
          +> serverCallCount.assert(2)
          +> errorMsg.assert(Some("Failed to connect"))
          +> sendButtonEnabled.assert(true)
          +> feedbackEnabled.assert(true)

          >> setNextServerResponse(Some(\/-(())))
          >> clickSend
          +> serverCallCount.assert(3)
          +> errorMsg.assert(None)
          +> sendButtonEnabled.assert(false) // submit succeeds so modal *would* close triggering a reset on completion
          +> serverLastFeedback.assert(Some("omgOMFG12345"))
      )

    'inFlight -
      runTest(None)(
        enterFeedback("qweasdzcQWEASDZXC123!@#")
          >> clickSend
          +> serverCallCount.assert(1)
          +> serverLastFeedback.assert(Some("qweasdzcQWEASDZXC123!@#"))
          +> sendButtonEnabled.assert(false)
          +> feedbackEnabled.assert(false)
          +> errorMsg.assert(None)
      )

  }
}
