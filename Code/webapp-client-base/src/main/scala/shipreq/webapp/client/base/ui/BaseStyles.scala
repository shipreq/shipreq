package shipreq.webapp.client.base.ui

import japgolly.scalajs.react.vdom.prefix_<^.{^ => ^^, _}
import scalacss.Defaults._

object BaseStyles extends StyleSheet.Inline {
  import dsl._

  val projectItems = new ProjectItems
  final class ProjectItems {

    def vspace = 1 rem

    val item = style(
      display.flex,
      padding(vspace, `0`),
      borderTop(1 px, solid, rgba(34, 36, 38, .15)), // .ui.divided.items>.item
      &.not(_.hover)(
        unsafeChild("a")(
          color(rgba(0, 0, 0, .85))))) // .ui.items>.item>.content>.header

    val itemLeft = style(
      flexGrow(1))

    val itemHeader = style(
      addClassNames("ui", "header"),
      marginBottom(`0`))

    // .ui.items>.item .meta
    val itemMeta = style(
      margin(0.5 em, `0`),
      fontSize(1 em),
      lineHeight(1 em),
      color(rgba(0, 0, 0, 0.6)))
  }

  val navBarContainer =
    ^^.marginBottom := "5rem"

  val breadcrumbDivider =
    (^^.color       := "#ddd") +
    (^^.marginLeft  := "0.8em") +
    (^^.marginRight := "0.8em")

  val maxWidthContainer =
    (^^.marginLeft   := "auto") +
    (^^.marginRight  := "auto") +
    (^^.paddingLeft  := "1em") +
    (^^.paddingRight := "1em") +
    (^^.maxWidth     := "calc(723px + 2em)") +
    (^^.width        := "100%")
}
