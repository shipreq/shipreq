package shipreq.webapp.base.ui.widgets

import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.ui.semantic.{Icon, Message}

/** When there is no content on a page, display this message rather than just emptiness.
  *
  *  The reason this exists here rather than just calling [[Message]] directly, is so that the theme is consistent in
  *  all cases.
  */
object NoContentMessage {

  def apply(title: TagMod, subtitle: TagMod): VdomTag =
    Message(
      Message.Style(Message.Type.Info),
      Icon.InfoCircle,
      title,
      subtitle)

  def becauseAllDead(title: TagMod, subtitle: TagMod): VdomTag =
    Message(
      Message.Style(Message.Type.Warning),
      Icon.TrashOutline,
      title,
      subtitle)

  def becauseAllDead(subtitle: TagMod): VdomTag =
    becauseAllDead("No live content.", subtitle)

  def becauseOfFilter(title: TagMod, subtitle: TagMod): VdomTag =
    Message(
      Message.Style(Message.Type.Info),
      Icon.Filter,
      title,
      subtitle)

  def becauseNotFound(title: TagMod, subtitle: TagMod): VdomTag =
    Message(
      Message.Style(Message.Type.Error),
      Icon.WarningSign,
      title,
      subtitle)

  def becauseAnErrorOccurred(title: TagMod, subtitle: TagMod): VdomTag =
    Message(
      Message.Style(Message.Type.Error),
      Icon.WarningCircle,
      title,
      subtitle)
}
