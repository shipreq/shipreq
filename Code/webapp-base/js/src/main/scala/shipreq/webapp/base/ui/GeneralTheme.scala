package shipreq.webapp.base.ui

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import scalaz.\/
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.data.{Disabled, Enabled}
import shipreq.webapp.base.lib.KeyHandler.Criterion
import shipreq.webapp.base.lib.KeyHandlers
import shipreq.webapp.base.ui.semantic.{Button, Colour, Size, UsesSemanticUiManually}
import shipreq.webapp.base.validation.Simple

object GeneralTheme {

  def showErrorMsg(e: ErrorMsg): Callback =
    Callback.alert(e.value)

  def renderSimpleInvalidity(d: Simple.Invalidity \/ Any): Option[VdomTag] =
    d.fold(i => Some(renderSimpleInvalidity(i)), _ => None)

  def renderSimpleInvalidity(i: Simple.Invalidity): VdomTag = {
    def render1(err: String): VdomTag = <.span(err)
    if (i.tail.isEmpty)
      render1(i.head)
    else
      <.div(MutableArray(i.whole).sort.iterator.map(render1).intersperse(<.br).toTagMod)
  }

  def submitButton(title: String, submitCB: Option[Callback], inFlight: Boolean): VdomTagOf[html.Button] = {

    // Call preventDefault so underlying form onSubmit handlers don't reload the page
    val onClick: ReactEvent => Callback =
      _.preventDefaultCB >> submitCB.getOrEmpty

    Button(
      state = if (inFlight) Button.State.Loading else Button.State.enabledWhen(submitCB.isDefined),
      colour = Colour.Blue,
      size = Size.Large,
    ).tag(
      ^.marginRight := "0",
      title,
      ^.onClick ==> onClick)
  }

  def submitOnEnter(submit: Callback): KeyHandlers =
    Criterion.Enter.handle(submit) + Criterion.CtrlEnter.handle(submit)

  def submitOnCtrlEnter(submit: Callback): KeyHandlers =
    Criterion.CtrlEnter.handle(submit).toKeyHandlers

  /** When using Semantic UI modals, we have to manually modify DOM rather than using React.
    * Herein lie helpers...
    */
  @UsesSemanticUiManually
  object nonReact {

    def setStateOfSubmitButton(dom: html.Button)(enabled: Enabled, inFlight: Boolean): Unit = {
      for (d <- Option(dom)) {
        d.disabled = enabled.is(Disabled)
        if (inFlight)
          d.classList.add("loading")
        else
          d.classList.remove("loading")
      }
    }

  }
}
