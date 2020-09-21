package shipreq.webapp.base.ui

import japgolly.scalajs.react.vdom.html_<^._

object InlineBaseStyles {

  final val pageMarginRem = 1
  final val pageMarginStr = pageMarginRem.toString + "rem"
  final val scrollBarSize = "1rem"

  val containerLarge = TagMod(
    ^.marginLeft.auto,
    ^.marginRight.auto,
    ^.maxWidth := s"calc(723px + $pageMarginStr + $pageMarginStr)")

  object layout {
    val root = TagMod(
      ^.display.flex,
      ^.flexDirection.column,
      ^.alignItems.stretch,
      ^.minHeight := "100%")

    val navMenu = TagMod(
      ^.borderRadius := "0")

    val navBreadcrumbDivider = TagMod(
      ^.color       := "#ddd",
      ^.marginLeft  := "0.8em",
      ^.marginRight := "0.8em")

    val main = TagMod(
      ^.flex         := "1",
      ^.marginTop    := pageMarginStr,
      ^.marginBottom := pageMarginStr,
      // ^.padding      := s"0 $pageMarginStr", // Flex parent padding doesn't work as expected - child margins required
      ^.width        := "100%",
    )

    val mainInner = TagMod(
      ^.flexGrow := "1",
      ^.marginLeft := pageMarginStr,
      ^.marginRight := pageMarginStr,
      ^.width := s"calc(100vw - 2 * $pageMarginStr - $scrollBarSize)",
    )
  }
}
