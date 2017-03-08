package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._, vdom.html_<^._, ScalazReact._
import scalaz.Equal
import shipreq.webapp.client.base.data.{Disabled, Enabled}

object SelectInvoke {

  def Component[A: Equal](name: String) =
    ScalaComponent.build[Props[A]](name)
      .render_P(render(_))
      .build

  final case class Props[A](selection  : SelectOne.Props[A],
                            invoke     : Option[Callback],
                            buttonLabel: String,
                            enabled    : Enabled)

  def render[A: Equal](p: Props[A]): VdomTag = {
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
