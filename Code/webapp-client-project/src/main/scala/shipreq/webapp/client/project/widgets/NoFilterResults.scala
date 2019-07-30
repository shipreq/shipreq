package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.ui.semantic.{Icon, Message}
import shipreq.webapp.client.project.app.Style

object NoFilterResults {

  def render: VdomTag =
    Message(
      Message.Style(Message.Type.Info),
      Icon.Filter,
      "No filter results.",
      "None of the project content matches the specified filter criteria.")

  def asTableRow(cols: Int) =
    <.tr(<.td(Style.noFilterResultsCont, ^.colSpan := cols, render))
}
