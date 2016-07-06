package shipreq.webapp.client.base.ui

import japgolly.scalajs.react.vdom.prefix_<^.{^ => ^^, _}
import scalacss.Defaults._

object BaseStyles extends StyleSheet.Inline {
  import dsl._

  val inlineEdit = style(
    &.hover(
      cursor.pointer,
      outline(dotted, 2 px, c"#FFA622"),
      backgroundColor(c"#fff4e3").important))

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

    val itemHeaderRW = style(
      itemHeaderRO,
      color(c"#d00").important)

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
  val editorInstructions = new EditorInstructions
  class EditorInstructions {

    private def textColour = color(c"#a4a6a8")

    val container = style(
      fontSize(0.75 rem),
      lineHeight(1 em),
      textColour,
      textAlign.right,
      marginBottom(editorInstructionMarginV))

    val clause = style(
      &.not(_.lastChild)(
        marginRight(1.1 ex)))

    val link = style(
      cursor.pointer,
      &.not(_.hover)(
        textColour.important, // important because of unsafeChild used in item's &.not(_.hover)(unsafeChild("a"))
        borderBottom(solid, 1 px, c"#e0e2e4")),
      &.hover(
        color(c"#2185D0"),
        // color(c"#525456"),
        textDecoration := "underline"))
  }

  def pageMargin = 1 rem
  def pageMarginStr = "1rem"

  val containerFull = style(
    margin.horizontal(pageMargin),
    marginBottom(pageMargin))

  val navBarContainer =
    ^^.marginBottom := "5rem"

  val breadcrumbDivider =
    (^^.color       := "#ddd") +
    (^^.marginLeft  := "0.8em") +
    (^^.marginRight := "0.8em")

  val containerLarge =
    (^^.marginLeft   := "auto") +
    (^^.marginRight  := "auto") +
    (^^.marginBottom := pageMarginStr) +
    (^^.paddingLeft  := "1em") +
    (^^.paddingRight := "1em") +
    (^^.maxWidth     := "calc(723px + 2em)") +
    (^^.width        := "100%")
}
