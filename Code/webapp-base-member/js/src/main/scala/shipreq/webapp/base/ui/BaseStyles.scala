package shipreq.webapp.base.ui

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.univeq._
import scala.concurrent.duration._
import shipreq.base.util.Validity
import shipreq.webapp.base.CssSettings._
import shipreq.webapp.base.data.{Off, On}
import shipreq.webapp.base.feature.PreviewFeature.Position
import shipreq.webapp.base.ui.semantic.{Colour, Label}

object BaseStyles extends StyleSheet.Inline {
  import dsl._

  /** Domains */
  object D {
    val on                 = Domain.ofValues[On](On, Off)
    val editorMode         = Domain.ofValues(EditTheme.Mode.values.whole: _*)
    val previewPosition    = Domain.ofValues(Position.values.whole: _*)
    val editorPosMode      = previewPosition *** editorMode
    val editorStatePosMode = (EditorState.domain *** previewPosition.option *** editorMode).map { case ((a, b), c) => (a, b, c) }
  }

  object ZIndex {
    final val fullscreen          = 1000
    final val previewToggleButton = 1001
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
    val backgroundColor = c"#fffce5"
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

  private val fullscreenEditorAndPreviewHeight =
    "calc(50vh - (" + editorInstructions.heightEm + "em / 2) - " + fullscreenPaddingEx + "ex)"

  val textEditor = styleF(D.editorStatePosMode) { case (state, pos, mode) =>
    import EditTheme.Mode
    styleS(
      width(100 %%),
      margin(`0`),
      padding(.3 em,.4 em),
      outlineStyle.none,
      boxShadow := "0 0 0 0 rgba(0, 0, 0, 0) inset",
      transition := "color .1s ease,border-color .1s ease",
      fontSize(1 em),
      lineHeight(1.2857),
      (mode, pos) match {

        case (Mode.Inline, Some(Position.Right)) =>
          val h = "calc(100vh - " + (editorInstructions.heightEm + editorInstructions.marginEm) + "em)"
          styleS(maxHeight :=! h)

        case (Mode.Inline, Some(Position.Under)) =>
          styleS(maxHeight(50 vh))

        case (Mode.Inline, None) =>
          styleS(maxHeight(90 vh))

        case (Mode.Fullscreen, None | Some(Position.Right)) =>
          val h = "calc(100vh - " + editorInstructions.heightEm + "em - 2 * (" + fullscreenPaddingEx + "ex))"
          styleS(minHeight :=! h, maxHeight :=! h, height :=! h)

        case (Mode.Fullscreen, Some(Position.Under)) =>
          val h = fullscreenEditorAndPreviewHeight
          styleS(minHeight :=! h, maxHeight :=! h, height :=! h)
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
      mixinIf(mode != EditTheme.Mode.Fullscreen)(
        &.focus(
          (state match {
            case EditorState.Valid     => focus.glowBorder
            case EditorState.Invalid   => styleS(boxShadow := "0 0 1ex rgba(224,180,180,.5)")
            case EditorState.InTransit => styleS()
          }): StyleS
        )
      )
    )
  }

  val textEditorInTransitValue = style(
    marginLeft(0.8 ex),
    whiteSpace.pre,
    flexGrow(1),
    opacity(0.5))

  val textEditorTopPreviewUnder = styleF(D.editorMode)(mode => styleS(
    display.flex,
    flexDirection.column,
    flexWrap.nowrap,
    alignItems.stretch,
    justifyContent.spaceBetween,
    width(100 %%),
    mode match {
      case EditTheme.Mode.Inline     => styleS()
      case EditTheme.Mode.Fullscreen => styleS(height(100 %%))
    }
  ))

  val textEditorLeftPreviewRight = styleF(D.editorMode)(mode => styleS(
    display.flex,
    flexWrap.nowrap,
    alignItems.stretch,
    justifyContent.spaceBetween,
    width(100 %%),
    unsafeChild(">div")(
      width :=! "calc(50% - 0.35rem)"
    ),
    mode match {
      case EditTheme.Mode.Inline     => styleS()
      case EditTheme.Mode.Fullscreen => styleS(height :=! s"calc(100vh - 2 * (" + fullscreenPaddingEx + "ex))")
    }
  ))

  val previewToggleWrapper1 = style(
    position.relative)

  val previewToggleWrapper2 = style(
    position.absolute,
    animationDuration(400 millis),
    top(.8 rem),
    right(.8 rem),
    zIndex(ZIndex.previewToggleButton),
  )

  private val previewButtonGap = 1.em

  val previewButtonsWhenRight = style(
    display.flex,
    flexDirection.column,
    unsafeChild(">*")(
      &.not(_.firstChild)(
        marginTop(previewButtonGap)
      )
    )
  )

  val previewButtonsWhenUnder = style(
    unsafeChild(">*")(
      &.not(_.firstChild)(
        marginLeft(previewButtonGap)
      )
    )
  )

  private final val fullscreenPaddingEx = 1

  val fullscreen = style(
    position.fixed,
    top(`0`),
    left(`0`),
    width(100 vw),
    height(100 vh),
    maxWidth(100 vw),
    maxHeight(100 vh),
    padding(fullscreenPaddingEx.ex),
    background := "#fff",
    zIndex(ZIndex.fullscreen),
  )

  val errorPointingUp = Label.Style(Label.Type.PointingUp, Colour.Red).div

  val errorAndInstructions = styleF(D.previewPosition)(pos => styleS(
    display.flex,
    justifyContent.spaceBetween,
    pos match {
      case Position.Right => styleS()
      case Position.Under => styleS(marginBottom(1 em))
    }
  ))

  val richTextPreview = styleF(D.editorPosMode) { case (pos, mode) => styleS(
    (pos, mode) match {
      case (Position.Right, _)                         => styleS(height(100 %%))
      case (Position.Under, EditTheme.Mode.Inline)     => styleS()
      case (Position.Under, EditTheme.Mode.Fullscreen) => styleS(height :=! fullscreenEditorAndPreviewHeight)
    },
    (backgroundImage := "repeating-linear-gradient(-225deg,rgba(0,0,0,0),rgba(0,0,0,0)5ex,rgba(137,214,229,.05)5ex,rgba(137,214,229,.05)10ex)").important,
    addClassNames("ui", "segments", "raised"),
  )}

  val richTextPreviewHeader = style(
    addClassNames("ui", "segment", "inverted"),
    paddingLeft(1 ex).important,
    paddingTop(0.3 em).important,
    paddingBottom(0.3 em).important,
    (background := c"#ccf6ff").important,
    color(c"#0d1516").important)

  val richTextPreviewBodyOuter = style(
    padding(1 ex).important,
    (background := "#0000").important,
    addClassNames("ui", "segment"))

  val richTextPreviewBodyInner = styleF(D.previewPosition)(pos => styleS(
    minHeight(1.4 em),
    pos match {
      case Position.Right => styleS(maxHeight :=! "calc(100vh - (1.4285em + (.3em + 1ex) * 2))")
      case Position.Under => styleS(maxHeight(33.33333 vh))
    },
    overflowY.auto,
  ))

  // ctrl-enter to save, esc to cancel.
  object editorInstructions {
    private def textColour = color(c"#a4a6a8")

    private[BaseStyles] final val heightEm = 1.5

    private[BaseStyles] final val marginEm = 0.4

    val container = styleF(D.editorMode)(mode => styleS(
      fontSize(0.75 rem),
      lineHeight(heightEm.em),
      textColour,
      textAlign.right,
      mode match {
        case EditTheme.Mode.Fullscreen => styleS()
        case EditTheme.Mode.Inline     => styleS(marginBottom(marginEm.em))
      },
    ))

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

    val fullscreenIcon = helpIcon
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
