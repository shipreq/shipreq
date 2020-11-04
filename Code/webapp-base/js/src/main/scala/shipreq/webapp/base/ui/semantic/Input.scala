package shipreq.webapp.base.ui.semantic

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import shipreq.base.util._
import shipreq.webapp.base.util.On

object Input {
  private[this] val disabled = ^.cls := "disabled"
  private[this] val error = ^.cls := "error"

  val Base        = divCls("ui input")
  val Error       = Base(error)
  val Action      = Base(^.cls := "action")
  val ActionError = Action(error)

  val errorAttr = VdomAttr.elidable("data-err")

  val validationErr = TagMod(
    ^.color      := "#9f3a38",
    ^.paddingTop := "0.15rem",
    ^.fontSize   := "92%")

  object Text {

    def apply(input   : TagMod,
              enabled : Enabled  = Enabled,
              validity: Validity = Valid,
             ): VdomTag =
      Base(
        disabled.when(enabled is Disabled),
        error.when(validity is Invalid),
        <.input.text(input))

    def withError(input     : TagMod,
                  error     : Option[VdomTag],
                  afterInput: VdomNode = EmptyVdom,
                  enabled   : Enabled = Enabled,
                 ): TagMod = {
      val base = TagMod(apply(input, enabled, Valid when error.isEmpty), afterInput)
      error match {
        case None      => base
        case Some(err) => TagMod(base, <.div(errorAttr := "1", validationErr, err))
      }
    }

    /** Text input with:
      * - icon inside on the left
      */
    def icon(icon: VdomTag, input: VdomTag, validity: Validity = Valid): VdomTag = {
      var r = Base(^.cls := "left icon", input, icon)
      if (validity is Invalid)
        r = r(^.cls := "error")
      r
    }

    /** Text input with:
      * - icon inside on the left
      * - something (usually a button) attached to the right outside
      */
    def iconAndRightAction(icon: VdomTag, input: VdomNode, right: TagMod, validity: Validity = Valid): VdomTag = {
      var r = Base(^.cls := "left icon right action", icon, input, right)
      if (validity is Invalid)
        r = r(^.cls := "error")
      r
    }

    def withRightButtons(input: VdomTagOf[html.Input], buttons: VdomTagOf[html.Button]*): VdomTag =
      <.div(^.cls := "ui action input", input)(buttons: _*)

    def loadingDisabled(value: String, icon: Icon = Icon.Search) =
      Base(^.cls := "loading icon",
        <.input.text(^.value := value, ^.disabled := true),
        icon.tag)
  }

  // ===================================================================================================================

  object Checkbox {

    def apply(on    : On,
              change: On => Callback,
              label : TagMod): VdomTag =
      apply(on, change, Some(label))

    def apply(on    : On,
              change: On => Callback,
              label : Option[TagMod]): VdomTag =
      apply(on, change, label, Enabled)

    def apply(on     : On,
              change : On => Callback,
              label  : Option[TagMod],
              enabled: Enabled): VdomTag = {
      val toggle = enabled match {
        case Enabled  => change(!on)
        case Disabled => Callback.empty
      }
      <.div(^.cls := "ui checkbox",
        <.input.checkbox(
          ^.checked := on.is(On),
          ^.disabled := enabled.is(Disabled),
          ^.onChange --> toggle,
        ),
        label.whenDefined(
          <.label(
            ^.cursor.pointer.when(enabled is Enabled),
            ^.onClick --> toggle,
            _)))
    }

    def fromStateSnapshot[S](ss     : StateSnapshot[On],
                             label  : TagMod,
                             enabled: Enabled = Enabled,
                            ): VdomTag =
      apply(
        on      = ss.value,
        change  = ss.setState,
        label   = Some(label),
        enabled = enabled
      )
  }
}
