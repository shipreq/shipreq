package shipreq.webapp.base.ui.semantic

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Lens
import org.scalajs.dom.html
import shipreq.base.util._
import shipreq.webapp.base.data.On

object Input {
  val Base        = divCls("ui input")
  val Error       = Base(^.cls := "error")
  val Action      = Base(^.cls := "action")
  val ActionError = Action(^.cls := "error")

  object Text {

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
    def iconAndRightAction(icon: VdomTag, input: VdomTag, right: TagMod, validity: Validity = Valid): VdomTag = {
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

  object Checkbox {

    def apply(on    : On,
              change: On => Callback,
              label : TagMod): VdomTag =
      apply(on, change, Some(label))

    def apply(on    : On,
              change: On => Callback,
              label : Option[TagMod]): VdomTag = {
      val toggle = change(!on)
      <.div(^.cls := "ui checkbox",
        <.input.checkbox(^.checked := on.is(On), ^.onChange --> toggle),
        label.whenDefined(<.label(^.cursor.pointer, ^.onClick --> toggle, _)))
    }

    /** Note: DO NOT use this with Reusability.
      * StateSnapshot + Lens + Reusability = NO!
      */
    def fromStateSnapshot[S](lens: Lens[S, Boolean],
                             ss: StateSnapshot[S],
                             label: TagMod): VdomTag =
      apply(
        On when lens.get(ss.value),
        v => ss.modState(lens set v.is(On)),
        label)
  }
}
