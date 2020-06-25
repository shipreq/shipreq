package shipreq.webapp.base.ui

import japgolly.microlibs.adt_macros.AdtMacros
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq.UnivEq
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.feature.{EditorStatus, PreviewFeature}
import shipreq.webapp.base.ui.semantic.Icon
import shipreq.webapp.base.ui.{BaseStyles => *}

object EditTheme {

  private[this] val editableInline: TagMod =
    TagMod(
      *.inlineEdit,
      ^.title := UiText.doubleClickToEdit)

  def editableInline(startEdit: Callback): TagMod =
    TagMod(editableInline, ^.onDblClick --> startEdit)

  def editableInline(startEdit: Option[Callback]): TagMod =
    startEdit.fold(TagMod.empty)(editableInline(_))

  def autosizeTextareaProps(style: Style, validity: Validity, value: String, tagMod: TagMod): TagMod =
    TagMod(
      BaseStyles.textEditor((validity, style.position)),
      ^.value := value,
      tagMod)

  def onTextareaEditorMount(ref: Ref.ToScalaComponent[_, _, _], autoFocus: CallbackTo[Boolean]): CallbackOption[Unit] =
    for {
      af <- autoFocus
      _  <- CallbackOption.require(af)
      _  <- onTextareaEditorMount(ref)
    } yield ()

  def onTextareaEditorMount(ref: Ref.ToScalaComponent[_, _, _]): CallbackOption[Unit] =
    ref.get.flatMap(m => onTextareaEditorMount(m.getDOMNode))

  def onTextareaEditorMount(cd: ComponentDom): CallbackOption[Unit] =
    (for {
      h <- CallbackOption.liftOption(cd.toHtml)
    } yield {
      val t = h.domCast[html.TextArea]
      val l = t.value.length
      t.setSelectionRange(l, l)
    }).toCallback

  val spinner: VdomTag =
    Icon.CircleNotched.loading.tag(^.marginRight := "0")

  sealed trait Position
  object Position {
    case object Under extends Position
    case object Right extends Position

    implicit def univEq: UnivEq[Position] = UnivEq.derive
    implicit def reusability: Reusability[Position] = Reusability.by_==
    val values = AdtMacros.adtValues[Position]
  }

  sealed trait OpenPreview
  object OpenPreview {

    /** Follows the logic described in [[PreviewFeature]] to only show preview when required, and with minimal change */
    case object Minimally extends OpenPreview

    /** Preview always shown. */
    case object Always extends OpenPreview

    /** Preview never shown. */
    case object Never extends OpenPreview

    /** Preview shown anytime `wantOpen` is `true`. */
    case object WhenWanted extends OpenPreview

    /** Preview shown by default, and can be manually toggled on/off. */
    case object ShowWithToggle extends OpenPreview

    implicit def univEq: UnivEq[OpenPreview] = UnivEq.derive
    implicit def reusability: Reusability[OpenPreview] = Reusability.by_==
  }

  final case class Style(position: Position, openPreview: OpenPreview)

  object Style {
    val default = Style(Position.Under, OpenPreview.Minimally)

    implicit def univEq: UnivEq[Style] = UnivEq.derive
    implicit def reusability: Reusability[Style] = Reusability.byRef || Reusability.derive
  }

  def renderEditor(status      : EditorStatus,
                   editor      : Validity => VdomElement,
                   readOnlyView: => VdomNode,
                   instructions: => TagMod): VdomTag =
    renderEditor(
      status       = status,
      editor       = editor,
      readOnlyView = readOnlyView,
      instructions = instructions,
      style        = Style.default,
      previewRW    = PreviewFeature.ReadWrite.Single.neverShow,
      preview      = EmptyVdom,
    )

  def renderEditor(status      : EditorStatus,
                   editor      : Validity => VdomElement,
                   readOnlyView: => VdomNode,
                   instructions: => TagMod,
                   style       : Style,
                   previewRW   : => PreviewFeature.ReadWrite.Single,
                   preview     : => TagMod): VdomTag = {

    status match {
      case EditorStatus.Ignore | EditorStatus.Valid(_) =>
        style.position match {
          case Position.Under => renderActiveUnder(editor, instructions, preview)
          case Position.Right => renderActiveRight(previewRW, editor, instructions, preview, style.openPreview)
        }

      case EditorStatus.Invalid(err) =>
        <.div(
          editor(Invalid), // TODO add error background
          *.errorPointingUp(err),
          preview)

      case EditorStatus.AsyncError(err, _, _) =>
        <.div(
          editor(Valid),
          *.errorPointingUp(err),
          preview)

      case EditorStatus.InTransit =>
        <.div(*.textEditor((*.EditorState.InTransit, style.position)),
          <.div(spinner),
          <.div(*.textEditorInTransitValue, readOnlyView))
    }
  }

  private def renderActiveUnder(editor      : Validity => VdomElement,
                                instructions: => TagMod,
                                preview     : => TagMod): VdomTag =
    <.div(
      editor(Valid),
      instructions,
      preview)

  private def renderActiveRight(previewRW   : => PreviewFeature.ReadWrite.Single,
                                editor      : Validity => VdomElement,
                                instructions: => TagMod,
                                preview     : => TagMod,
                                openPreview : OpenPreview): VdomTag = {

    def renderWithPreview: VdomTag =
      <.div(*.textEditorLeftPreviewRight,
        <.div(editor(Valid), instructions),
        <.div(preview))

    def renderWithoutPreview: VdomTag =
      <.div(editor(Valid), instructions)

    @inline def render(preview: Boolean): VdomTag =
      if (preview) renderWithPreview else renderWithoutPreview

    def manual(defaultShow: Boolean) = {
      val show = previewRW.read.showManuallyControlledPreview(defaultShow)
      val inner = render(preview = show)
      previewRW.toggleButton(defaultShow = defaultShow)(inner)
    }

    openPreview match {
      case OpenPreview.Minimally
         | OpenPreview.Always
         | OpenPreview.Never
         | OpenPreview.WhenWanted     => renderWithPreview
      case OpenPreview.ShowWithToggle => manual(true)
    }
  }

  def renderPreview(p       : => PreviewFeature.ReadWrite.Single,
                    style   : Style,
                    wantOpen: => Boolean,
                    view    : => VdomNode): VdomNode = {
    def render =
      <.div(*.richTextPreview(style.position),
        <.div(*.richTextPreviewHeader, "Preview"),
        <.div(*.richTextPreviewBodyOuter,
          <.div(*.richTextPreviewBodyInner(style.position), view)))

    def manual(defaultShow: Boolean) =
      if (p.read.showManuallyControlledPreview(defaultShow))
        render
      else
        EmptyVdom

    style.openPreview match {
      case OpenPreview.Minimally      => p.reactCollapse(wantOpen)(render)
      case OpenPreview.Always         => render
      case OpenPreview.Never          => EmptyVdom
      case OpenPreview.WhenWanted     => PreviewFeature.ReadWrite.Single.show(wantOpen).reactCollapse(wantOpen)(render)
      case OpenPreview.ShowWithToggle => manual(true)
    }
  }
}
