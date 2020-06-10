package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.base.util.IsoBool
import shipreq.webapp.base.data.On

object Checkbox {

  def apply[A <: IsoBool[A]](bool: IsoBool[A])(set: A => Callback, decor: A => VdomTag => VdomElement) = {
    implicit val reusability = Reusability.by(bool.from)
    val on = On fnToThisWhen bool

    ScalaComponent.builder[A]
      .render_P { a =>
        val t = Widgets.checkbox(on(a))(^.onChange --> set(!a))
        decor(a)(t)
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
  }
}