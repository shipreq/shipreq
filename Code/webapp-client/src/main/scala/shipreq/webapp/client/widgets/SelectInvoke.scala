package shipreq.webapp.client.widgets

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import org.scalajs.dom.raw.HTMLDivElement
import scalaz.Equal
import shipreq.webapp.client.data.{Disabled, Enabled}

object SelectInvoke {

  def Component[A: Equal](name: String) =
    ReactComponentB[Props[A]](name)
      .render_P(render(_))
      .domType[HTMLDivElement]
      .build

  final case class Props[A](selection  : SelectOne.Props[A],
                            invoke     : Option[Callback],
                            buttonLabel: String,
                            enabled    : Enabled)

  def render[A: Equal](p: Props[A]): ReactTag = {
    val disabled = p.enabled :: Disabled

    val select = {
      // Propagate disabledness
      var q = p.selection
      if (disabled && q.select.isDefined)
        q = q.copy(select = None)

      SelectOne.render(q)
    }

    val invokeButton =
      <.button(
        ^.disabled  := (disabled || p.invoke.isEmpty),
        ^.onClick -->? p.invoke,
        p.buttonLabel)

    <.div(select, invokeButton)
  }
}
