package shipreq.webapp.client.app.ui

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.extra._
import scalaz.effect.IO
import shipreq.base.util.IsoBool
import shipreq.webapp.client.lib.{ShowDead, FilterDead}
import shipreq.webapp.client.lib.ui.UI
import shipreq.webapp.client.util.On

object Checkbox {

  def apply[A](bool: IsoBool[A])(set: A => IO[Unit], decor: A => ReactTag => ReactElement) = {
    implicit val reusability = Reusability.by(bool.from)
    val on = On when bool

    ReactComponentB[A]("Checkbox")
      .render { a =>
        val t = UI.checkbox(on(a))(^.onChange ~~> set(bool negate a))
        decor(a)(t)
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
  }

  def filterDead(set: FilterDead => IO[Unit]) =
    Checkbox(ShowDead)(set, _ => chk => <.label(chk, "Show deleted items."))

  def filterDead_$($: CompStateFocus[FilterDead]): () => ReactElement = {
    val component = filterDead($ setStateIO _)
    () => component($.state)
  }
}