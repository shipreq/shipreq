package shipreq.webapp.base.ui

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.univeq._
import shipreq.base.util.Validity
import shipreq.webapp.base.CssSettings._
import shipreq.webapp.base.data.{Off, On}
import shipreq.webapp.base.ui.semantic.{Colour, Label}

object BaseStyles extends StyleSheet.Inline {
  import dsl._

  /** Domains */
  object D {
    val on                = Domain.ofValues[On](On, Off)
    val editStylePosition = Domain.ofValues(EditTheme.Position.values.whole: _*)
    val editorStateAndPos = EditorState.domain *** editStylePosition
  }

  @inline def containerLarge = InlineBaseStyles.containerLarge
  @inline def layout         = InlineBaseStyles.layout

  val pageMargin = InlineBaseStyles.pageMarginRem.rem

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

  object toast {

    private final val transitionValue = ".36s cubic-bezier(0.4, 0, 0.2, 1)"

    val toast = styleF(D.on)(on => styleS(
      position.fixed,
      top(2.3 rem),
      left(50 %%),
      transform := "translate(-50%,0)",
      minWidth(10 vw),
      maxWidth(50 vw),
      textAlign.center,
      padding(1 em, 1.5 em),
      lineHeight(1.4285 em),
      borderRadius(.28571429 rem),
      background := "#dff0ff",
      border :=! "solid 1px #2185d0e8",
      boxShadow := "0 3px 5px -1px hsla(206,73%,32%,.1),0 6px 10px 0 hsla(206,73%,32%,.07),0 1px 18px 0 hsla(206,73%,32%,.06)",
      color(c"#2185d0"),
      transition := s"all $transitionValue",
      zIndex(1000),
      on match {
        case On  => mixin(visibility.visible, opacity(1))
        case Off => mixin(visibility.hidden, opacity(0))
      },
      unsafeChild("a")(color(c"#10617a")),
    ))

    val item = styleF(D.on)(on => styleS(
      transition := s"all $transitionValue",
      on match {
        case On  => mixin(visibility.visible, opacity(1))
        case Off => mixin(visibility.hidden, opacity(0))
      },
      &.not(_.firstChild)(
        marginTop(0.5 em),
      ),
    ))
  }
  toast // eager eval

  object focus {
    def colour(a: Double) = rgba(163, 51, 200, a)
    def BoxShadow = styleS(boxShadow := s"0 0 1ex ${colour(.45).value}")
    def BorderColour = colour(.75)

    def glowOutline = styleS(BoxShadow, outline(solid, 1 px, BorderColour))
    def glowBorder = styleS(BoxShadow, border(solid, 1 px, BorderColour))
  }
  focus // eager eval

  object editor {
    val backgroundColor = c"#fffad7" // = rgba(255, 227, 58, .2)
    val borderColor = rgba(239, 207, 9, .6)
  }
  editor // eager eval

  val inlineEdit = style(
    &.hover(
      cursor.pointer,
      backgroundColor(editor.backgroundColor).important))

  object projectItems {
    def vspace = 2 rem

    val item = style(
      display.flex,
      padding.horizontal(`0`),
      &.firstChild(
        paddingTop(`0`)),
      &.not(_.firstChild)(
        borderTop(1 px, solid, rgba(34, 36, 38, .15)), // .ui.divided.items>.item
        paddingTop(vspace)),
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
      color(c"#1e70bf").important)

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
  projectItems // eager eval

  val textEditor = styleF(D.editorStateAndPos) { case (state, pos) =>
    styleS(
      width(100 %%),
      margin(`0`),
      padding(.3 em,.4 em),
      outlineStyle.none,
      boxShadow := "0 0 0 0 rgba(0, 0, 0, 0) inset",
      transition := "color .1s ease,border-color .1s ease",
      fontSize(1 em),
      lineHeight(1.2857),
      pos match {
        case EditTheme.Position.Right => styleS(maxHeight :=! "calc(100vh - 2em)")
        case EditTheme.Position.Under => styleS(maxHeight(50 vh))
      },
      // overflow: scroll - autosize avoids this
      resize.none,
      color(state match {
        case EditorState.Valid
           | EditorState.InTransit => rgba(0, 0, 0, .87)
        case EditorState.Invalid   => c"#9F3A38"
      }),
      backgroundColor(state match {
        case EditorState.Valid     => editor.backgroundColor
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
           | EditorState.InTransit => editor.borderColor
        case EditorState.Invalid   => c"#E0B4B4"
      }),
      mixinIf(state ==* EditorState.InTransit)(display.flex),
      &.focus(
        (state match {
          case EditorState.Valid     => focus.glowBorder
          case EditorState.Invalid   => styleS(boxShadow := "0 0 1ex rgba(224,180,180,.5)")
          case EditorState.InTransit => styleS()
        }): StyleS
      )
    )
  }

  val textEditorInTransitValue = style(
    marginLeft(0.8 ex),
    whiteSpace.pre,
    flexGrow(1),
    opacity(0.5))

  val textEditorLeftPreviewRight = style(
    display.flex,
    flexWrap.nowrap,
    alignItems.stretch,
    justifyContent.spaceBetween,
    width(100 %%),
    unsafeChild(">div")(
      width :=! "calc(50% - 0.35rem)"
    )
  )

  val previewToggleWrapper = style(
    position.relative)

  val previewToggleButton = style(
    position.absolute,
    top(.8 rem),
    right(.8 rem),
    zIndex(1000),
  )

  val errorPointingUp = Label.Style(Label.Type.PointingUp, Colour.Red).div

  val richTextPreview = styleF(D.editStylePosition)(pos => styleS(
    pos match {
      case EditTheme.Position.Right => styleS(height(100 %%))
      case EditTheme.Position.Under => styleS()
    },
    addClassNames("ui", "segments", "raised")))

  val richTextPreviewHeader = style(
    addClassNames("ui", "segment", "inverted"),
    paddingLeft(1 ex).important,
    paddingTop(0.3 em).important,
    paddingBottom(0.3 em).important,
    (background := c"#ccf6ff").important,
    color(c"#0d1516").important)

  val richTextPreviewBodyOuter = style(
    padding(1 ex).important,
    addClassNames("ui", "segment"),
    (backgroundImage := "repeating-linear-gradient(-225deg,rgba(0,0,0,0),rgba(0,0,0,0)5ex,rgba(137,214,229,.05)5ex,rgba(137,214,229,.05)10ex)").important)

  val richTextPreviewBodyInner = styleF(D.editStylePosition)(pos => styleS(
    minHeight(1.4 em),
    pos match {
      case EditTheme.Position.Right => styleS(maxHeight :=! "calc(100vh - (1.4285em + (.3em + 1ex) * 2))")
      case EditTheme.Position.Under => styleS(maxHeight(33.33333 vh))
    },
    overflowY.auto,
  ))

  private def editorInstructionMarginV = 0.4 em

  // ctrl-enter to save, esc to cancel.
  object editorInstructions {
    private def textColour = color(c"#a4a6a8")

    val container = style(
      fontSize(0.75 rem),
      lineHeight(1.5 em),
      textColour,
      textAlign.right,
      marginBottom(editorInstructionMarginV))

    val clause = style(
      display.inlineBlock, // prevent word-wrap in the middle of a clause
      marginLeft(1 ex),
    )

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
      marginLeft(0.35 ex).important,
      marginRight(`0`).important,
      cursor.pointer,
      &.hover(color(hoverColour)))
  }

  val autoComplete = new AutoComplete
  final class AutoComplete {
    val itemTitle = style(
      fontWeight.bold)

    val itemTitle2 = style(
      paddingLeft(1 ex),
      color(c"#333"))

    val itemDesc = style(
      color(c"#444"),
      fontStyle.italic,
      overflow.hidden,
      maxWidth(36 ex))
  }
  editorInstructions // eager eval

  val cancelButton = style(
    (background := "#fff").important,
    borderColor(c"#27292a").important)
}
