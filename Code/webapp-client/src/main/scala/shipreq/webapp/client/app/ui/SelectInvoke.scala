package shipreq.webapp.client.app.ui

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import org.scalajs.dom.raw.HTMLDivElement
import scalaz.Equal
import scalaz.effect.IO
import shipreq.webapp.client.util.{Disabled, Enabled}

object SelectInvoke {

  def Component[A: Equal](name: String) =
    ReactComponentB[Props[A]](name)
      .render(render(_))
      .domType[HTMLDivElement]
      .build

  final case class Props[A](selection  : SelectOne.Props[A],
                            invoke     : Option[IO[Unit]],
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
        ^.onClick ~~>? p.invoke,
        p.buttonLabel)

    <.div(select, invokeButton)
  }
}
