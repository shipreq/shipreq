package shipreq.webapp.client.widgets

import japgolly.scalajs.react._, vdom.prefix_<^._
import japgolly.scalajs.react.extra._
import shipreq.base.util.IsoBool
import shipreq.webapp.base.UiText
import shipreq.webapp.client.data.{ShowDead, FilterDead, On}

object Checkbox {

  def apply[A <: IsoBool[A]](bool: IsoBool[A])(set: A => Callback, decor: A => ReactTag => ReactElement) = {
    implicit val reusability = Reusability.by(bool.from)
    val on = On when bool

    ReactComponentB[A]("Checkbox")
      .render_P { a =>
        val t = Widgets.checkbox(on(a))(^.onChange --> set(!a))
        decor(a)(t)
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
  }

  def filterDeadChecked = ShowDead

  def filterDead(set: FilterDead => Callback) =
    Checkbox(filterDeadChecked)(set, _ => chk => <.label(chk, UiText.Life.showDead))

  def filterDead_$($: CompState.Access[FilterDead]): () => ReactElement = {
    val component = filterDead($ setState _)
    () => component($.state.runNow())
  }
}