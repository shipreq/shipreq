package shipreq.webapp.base.ui

import japgolly.scalajs.react.vdom.html_<^.TagMod
import shipreq.webapp.base.ui.semantic.{Icon, Message}

/** When there is no content on a page, display this message rather than just emptiness.
  *
  *  The reason this exists here rather than just calling [[Message]] directly, is so that the theme is consistent in
  *  all cases.
  */
object NoContentMessage {

  def apply(header: TagMod, content: TagMod) =
    Message(
      Message.Style(Message.Type.Info),
      Icon.InfoCircle,
      header,
      content)

  def becauseAllDead(header: TagMod, content: TagMod) =
    Message(
      Message.Style(Message.Type.Warning),
      Icon.TrashOutline,
      header,
      content)

  def becauseOfFilter(header: TagMod, content: TagMod) =
    Message(
      Message.Style(Message.Type.Info),
      Icon.Filter,
      header,
      content)

  def becauseNotFound(header: TagMod, content: TagMod) =
    Message(
      Message.Style(Message.Type.Error),
      Icon.WarningSign,
      header,
      content)

  def becauseAnErrorOccurred(header: TagMod, content: TagMod) =
    Message(
      Message.Style(Message.Type.Error),
      Icon.WarningCircle,
      header,
      content)
}
