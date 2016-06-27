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
      padding.horizontal(`0`),
      &.firstChild(
        paddingTop(`0`)),
      &.not(_.firstChild)(
        borderTop(1 px, solid, rgba(34, 36, 38, .15)), // .ui.divided.items>.item
        paddingTop(vspace)),
      &.lastChild(
        paddingBottom(`0`)),
      &.not(_.lastChild)(
        paddingBottom(vspace)),
      &.not(_.hover)(
        unsafeChild("a")(
          color(rgba(0, 0, 0, .85))))) // .ui.items>.item>.content>.header

    val itemLeft = style(
      flexGrow(1),
      marginRight(1 rem).important) // so that there's a gap between this & the stats. Affects :hover & <input> size

    val itemHeaderRO = style(
      addClassNames("ui", "header"),
      marginBottom(`0`).important)

    private def inlineEdit = mixin(
      &.hover(
        cursor.pointer,
        outline(dotted, 2 px, c"#FFA622"),
        backgroundColor(c"#fff4e3").important))

    val itemHeaderRW = style(
      itemHeaderRO,
      color(c"#d00").important,
      inlineEdit)

    val itemHeaderEditCont = style(
      width(100 %%))

    // .ui.items>.item .meta
    val itemMeta = style(
      margin(0.5 em, `0`),
      fontSize(1 em),
      lineHeight(1 em),
      color(rgba(0, 0, 0, 0.6)))
  }

  private def editorInstructionMarginV = 0.4 em

  // ctrl-enter to save, esc to cancel.
  val editorInstructions = style(
    fontSize(0.75 rem),
    lineHeight(1 em),
    color(c"#a4a6a8"),
    &.not(_.firstChild)(marginTop(editorInstructionMarginV)),
    &.not(_.lastChild)(marginBottom(editorInstructionMarginV)),

    unsafeChild("a")(
      cursor.pointer,
      &.not(_.hover)(
        color.inherit,
        borderBottom(solid, 1 px, c"#e0e2e4")),
      &.hover(
        color(c"#525456"),
        textDecoration := "underline")))

  val fullWidthContainer = style(
    margin.horizontal(1 rem))

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
