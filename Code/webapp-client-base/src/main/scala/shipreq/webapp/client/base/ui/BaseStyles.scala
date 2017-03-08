package shipreq.webapp.client.base.ui

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.scalajs.react.vdom.html_<^.{^ => ^^, _}
import japgolly.univeq._
import scalacss.Defaults._
import shipreq.base.util.Validity
import shipreq.webapp.client.base.ui.semantic.{Colour, Label}

object BaseStyles extends StyleSheet.Inline {
  import dsl._

  sealed abstract class EditorState extends Product with Serializable
  object EditorState {
    case object Valid     extends EditorState
    case object Invalid   extends EditorState
    case object InTransit extends EditorState

    implicit def univEq: UnivEq[EditorState] = UnivEq.derive

    implicit def fromValidity(v: Validity): EditorState =
      v match {
        case shipreq.base.util.Valid   => Valid
        case shipreq.base.util.Invalid => Invalid
      }

    val domain =
      Domain.ofValues(AdtMacros.adtValues[EditorState].whole: _*)
  }

  val editorBackgroundColor = c"#fff4e3"
  val editorBorderColor = rgba(255, 166, 34, .5)
  val editorBorderColorFocus = rgb(255, 166, 34)

  val inlineEdit = style(
    &.hover(
      cursor.pointer,
      outline(dotted, 2 px, c"#FFA622"),
      backgroundColor(editorBackgroundColor).important))

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
      width(100 %%),
      marginBottom(0.2 rem))

    // .ui.items>.item .meta
    val itemMeta = style(
      margin(0.5 em, `0`),
      fontSize(1 em),
      lineHeight(1 em),
      color(rgba(0, 0, 0, 0.6)))
  }

  val textEditor = styleF(EditorState.domain) { state =>
    styleS(
      width(100 %%),
      margin(`0`),
      padding(.3 em,.4 em),
      outlineStyle.none,
      boxShadow := "0 0 0 0 rgba(0, 0, 0, 0) inset",
      transition := "color .1s ease,border-color .1s ease",
      fontSize(1 em),
      lineHeight(1.2857),
      // overflow: scroll - autosize avoids this
      resize.none,
      color(state match {
        case EditorState.Valid
           | EditorState.InTransit => rgba(0, 0, 0, .87)
        case EditorState.Invalid   => c"#9F3A38"
      }),
      backgroundColor(state match {
        case EditorState.Valid     => editorBackgroundColor
        case EditorState.Invalid   => c"#FFF6F6"
        case EditorState.InTransit => rgba(255,244,227,0.7)
      }),
      borderWidth(1 px),
      borderStyle(state match {
        case EditorState.Valid
           | EditorState.Invalid   => solid
        case EditorState.InTransit => dashed
      }),
      borderRadius(.28571429 rem),
      borderColor(state match {
        case EditorState.Valid
           | EditorState.InTransit => editorBorderColor
        case EditorState.Invalid   => c"#E0B4B4"
      }),
      mixinIf(state ==* EditorState.InTransit)(display.flex),
      &.focus(
        (state match {
          case EditorState.Valid     => styleS(borderColor(editorBorderColorFocus), boxShadow := s"0 0 1ex ${editorBorderColor.value}")
          case EditorState.Invalid   => styleS(boxShadow := "0 0 1ex rgba(224,180,180,.5)")
          case EditorState.InTransit => styleS()
        }): StyleS
      )
    )
  }

  val textEditorInTransitValue = style(
    whiteSpace.pre,
    flexGrow(1),
    opacity(0.5))

  val errorPointingUp = Label.Style(Label.Type.PointingUp, Colour.Red).div

  val richTextPreview = style(
    addClassNames("ui", "segments", "raised"))

  val richTextPreviewHeader = style(
    addClassNames("ui", "segment", "inverted", "green"),
    paddingTop(0.3 em).important,
    paddingBottom(0.3 em).important)

  val richTextPreviewBody = style(
    addClassNames("ui", "segment"),
    (backgroundImage := "repeating-linear-gradient(-225deg,rgba(0,0,0,0),rgba(0,0,0,0)5ex,rgba(33,186,67,.07)5ex,rgba(33,186,67,.07)10ex)")
      .important)

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

    private def hoverColour = c"#2185D0"

    val link = style(
      cursor.pointer,
      &.not(_.hover)(
        textColour.important, // important because of unsafeChild used in item's &.not(_.hover)(unsafeChild("a"))
        borderBottom(solid, 1 px, c"#e0e2e4")),
      &.hover(
        color(hoverColour),
        // color(c"#525456"),
        textDecoration := "underline"))

    val helpIcon = style(
      marginRight(`0`).important,
      cursor.pointer,
      &.hover(color(hoverColour)))
  }

  def pageMargin = 1 rem
  def pageMarginStr = "1rem"

  val containerFull = style(
    margin.horizontal(pageMargin),
    marginBottom(pageMargin))

  val navBarContainer =
    ^^.marginBottom := "5rem"

  val breadcrumbDivider = TagMod(
    ^^.color       := "#ddd",
    ^^.marginLeft  := "0.8em",
    ^^.marginRight := "0.8em")

  val containerLarge = TagMod(
    ^^.marginLeft   := "auto",
    ^^.marginRight  := "auto",
    ^^.marginBottom := pageMarginStr,
    ^^.paddingLeft  := "1em",
    ^^.paddingRight := "1em",
    ^^.maxWidth     := "calc(723px + 2em)",
    ^^.width        := "100%")
}
