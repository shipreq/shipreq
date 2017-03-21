package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._, vdom.html_<^._
import japgolly.scalajs.react.extra._
import shipreq.base.util.IsoBool
import shipreq.webapp.base.data.{FilterDead, ShowDead}
import shipreq.webapp.base.UiText
import shipreq.webapp.client.base.data.On

object Checkbox {

  def apply[A <: IsoBool[A]](bool: IsoBool[A])(set: A => Callback, decor: A => VdomTag => VdomElement) = {
    implicit val reusability = Reusability.by(bool.from)
    val on = On when bool

    ScalaComponent.builder[A]("Checkbox")
      .render_P { a =>
        val t = Widgets.checkbox(on(a))(^.onChange --> set(!a))
        decor(a)(t)
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
  }

  // TODO Deprecate Checkbox.filterDead
  def filterDeadChecked = ShowDead

  def filterDead(set: FilterDead => Callback) =
    Checkbox(filterDeadChecked)(set, _ => chk => <.label(chk, UiText.Life.showDead))

  def filterDead_$($: StateAccessPure[FilterDead]): () => VdomElement = {
    val component = filterDead($ setState _)
    () => component($.state.runNow())
  }
}