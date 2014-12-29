package shipreq.webapp.client.app.ui

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import scalaz.effect.IO
import shipreq.webapp.client.lib.ui.TypicalStoresAndState
import shipreq.webapp.client.lib.ui.UI.checkbox

object ShowDeletedToggler {

  def apply(show: Boolean, toggle: => IO[Unit]): ReactElement =
    <.label(
      checkbox(show)(^.onChange ~~> toggle),
      if (show) "Show deleted items." else "Showing deleted items.")

  def apply(sas: TypicalStoresAndState[_, _, _])(c: ComponentStateFocus[sas.S]): ReactElement =
    apply(c.state.showDeleted, c.runState(sas.toggleShowDeleted))
}