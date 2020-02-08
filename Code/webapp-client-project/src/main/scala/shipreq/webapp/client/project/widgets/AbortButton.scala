package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.ui.BaseStyles
import shipreq.webapp.base.ui.semantic.{Button, Colour, Icon}

sealed abstract class AbortButton(label: String) {

  val tag =
    Button(
      tipe = Button.Type.BasicIconAndText(Icon.Remove, label),
      colour = Colour.Black)
      .tag(BaseStyles.cancelButton)

  def apply(onClick: Callback) =
    tag(^.onClick --> onClick)

}

object CancelButton extends AbortButton("Cancel")
object CloseButton extends AbortButton("Close")
