package shipreq.webapp.client.app.ui

import japgolly.scalajs.react._, vdom.prefix_<^._
import japgolly.scalajs.react.extra._
import shipreq.base.util.IsoBool
import shipreq.webapp.client.lib.{ShowDead, FilterDead}
import shipreq.webapp.client.lib.ui.UI
import shipreq.webapp.client.util.On

object Checkbox {

  def apply[A](bool: IsoBool[A])(set: A ~=> Callback, decor: A => ReactTag => ReactElement) = {
    implicit val reusability = Reusability.by(bool.from)
    val on = On when bool

    ReactComponentB[A]("Checkbox")
      .render_P { a =>
        val t = UI.checkbox(on(a))(^.onChange --> set(bool negate a))
        decor(a)(t)
      }
      .configure(shouldComponentUpdate)
      .build
  }

  def filterDeadChecked = ShowDead

  def filterDead(set: FilterDead ~=> Callback) =
    Checkbox(filterDeadChecked)(set, _ => chk => <.label(chk, "Show deleted content."))

  def filterDead_$($: CompStateFocus[FilterDead]): () => ReactElement = {
    val component = filterDead(ReusableFn($).setState)
    () => component($.state)
  }
}