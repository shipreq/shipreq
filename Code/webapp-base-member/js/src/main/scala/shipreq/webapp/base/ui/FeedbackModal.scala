package shipreq.webapp.base.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.{Element, document, html}
import scalaz.{-\/, \/, \/-}
import shipreq.base.util.{ErrorMsg, OpResult}
import shipreq.webapp.base.data.{Disabled, Enabled}
import shipreq.webapp.base.lib.ModalForm
import shipreq.webapp.base.protocol.AjaxClient
import shipreq.webapp.base.protocol.CommonProtocols.{Metadata, SubmitFeedback}
import shipreq.webapp.base.ui.semantic.UsesSemanticUiManually
import shipreq.webapp.base.util.TextMod

/** Pops up a modal that asks a user for feedback.
  *
  * Usage:
  *
  * 1. Add `.render` to the root view.
  *    It will be hidden.
  *    It has reusability so as to only evaluate once.
  *
  * 2. Call `.run` to display the modal.
  */
final case class FeedbackModal(render: VdomElement, run: AsyncCallback[OpResult])

@UsesSemanticUiManually
object FeedbackModal {

  type SubmitFn = SubmitFeedback.UserInput => AsyncCallback[ErrorMsg \/ Unit]

  private[ui] val errorEmptyFeedback = ErrorMsg("You can't submit nothing as your feedback.")

  def apply(metadata: CallbackTo[Metadata.Client]): FeedbackModal =
    apply(metadata, AjaxClient.Binary)

  def apply(metadata: CallbackTo[Metadata.Client], ajaxClient: AjaxClient.Binary): FeedbackModal = {
    import SubmitFeedback._
    val f = ajaxClient.invoker(ajax).contramapInputCB((i: UserInput) => metadata.map(Request(i, _)))
    apply(f(_), document.body)
  }

  def apply(submitFeedback: SubmitFn,
            rootDom       : Element): FeedbackModal = {

    import ModalForm.SetState

    object modalForm extends ModalForm[OpResult]("FeedbackModal", OpResult.Failure, "Send", rootDom, OpResult.isSuccess) {

      val feedbackDom     = getDom[html.Input]("textarea")
      val feedbackGet     = feedbackDom.map(i => TextMod.multiLineWhitespace(i.value))
      val loginButtonDom  = getDom[html.Button](".button.primary")
      val errorMessageDom = getDom[html.Div](".ui.message")
      val errorMessage    = <.div(^.cls := "ui message error")

      override def setState(s: SetState): Callback =
        for {
          fd <- feedbackDom
          lb <- loginButtonDom
          em <- errorMessageDom
        } yield {
          for (d <- Option(fd)) {
            d.readOnly = s.form.is(Disabled)
          }
          for (d <- Option(em)) {
            d.style.display = if (s.error.isDefined) null else "none"
            d.innerHTML = s.error.fold("")(_.value)
          }
          GeneralTheme.nonReact.setStateOfSubmitButton(lb)(s.form, inFlight = s.inFlight)
        }

      override val clearFormData: Callback =
        feedbackDom.map(_.value = "")

      override val header: TagMod =
        "Send Feedback"

      override val content = TagMod(
        <.p("Thank you for taking a moment to give us feedback."),
        <.p("Whether it be a suggestion for improvement, a bug report, or even just sharing your experience, all feedback is appreciated."),
        <.div(
          ^.cls := "ui form",
          ^.paddingTop := "0.3333em",
          <.div(
            ^.cls := "field",
            <.textarea(
              ^.autoFocus := true,
              ^.placeholder := "What would you like to say?",
              ^.rows := 12,
              ^.onChange --> setState(SetState(Enabled, None, inFlight = false)),
              GeneralTheme.submitOnCtrlEnter(submit(None))))),
        errorMessage)

      override val justSubmit: AsyncCallback[SetState \/ OpResult] =
        feedbackGet.map(SubmitFeedback.UserInput).asAsyncCallback.flatMap { input =>
          if (input.feedback.isEmpty)
            AsyncCallback.pure(-\/(SetState(Enabled, Some(errorEmptyFeedback), inFlight = false)))
          else
            submitFeedback(input).map {
              case \/-(_) => \/-(OpResult.Success)
              case -\/(e) => -\/(SetState(Enabled, Some(e), inFlight = false))
            }
        }
    }

    FeedbackModal(modalForm.component(), modalForm.run)
  }

  implicit val reusability: Reusability[FeedbackModal] =
    Reusability.byRef
}
