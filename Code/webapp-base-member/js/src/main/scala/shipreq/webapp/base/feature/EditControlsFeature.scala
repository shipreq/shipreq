package shipreq.webapp.base.feature

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data.{Disabled, Enabled}
import shipreq.webapp.base.ui.{BaseStyles => *, _}

object EditControlsFeature {

  type ExtraControls = shipreq.webapp.base.feature.editcontrols.ExtraControls
  val  ExtraControls = shipreq.webapp.base.feature.editcontrols.ExtraControls

  val  Controls      = shipreq.webapp.base.feature.editcontrols.Controls

  type Font          = shipreq.webapp.base.feature.editcontrols.Font
  val  Font          = shipreq.webapp.base.feature.editcontrols.Font

  val  Keys          = shipreq.webapp.base.feature.editcontrols.Keys

  type Layout        = shipreq.webapp.base.feature.editcontrols.Layout
  val  Layout        = shipreq.webapp.base.feature.editcontrols.Layout

  type Mode          = shipreq.webapp.base.feature.editcontrols.Mode
  val  Mode          = shipreq.webapp.base.feature.editcontrols.Mode

  type OpenPreview   = shipreq.webapp.base.feature.editcontrols.OpenPreview
  val  OpenPreview   = shipreq.webapp.base.feature.editcontrols.OpenPreview

  type Style         = shipreq.webapp.base.feature.editcontrols.Style
  val  Style         = shipreq.webapp.base.feature.editcontrols.Style

  type WhenInTransit = shipreq.webapp.base.feature.editcontrols.WhenInTransit
  val  WhenInTransit = shipreq.webapp.base.feature.editcontrols.WhenInTransit

  final val defaultAbortVerb  = editcontrols.Instructions.defaultAbortVerb
  final val defaultCommitVerb = editcontrols.Instructions.defaultCommitVerb

  def spinner = shipreq.webapp.base.feature.editcontrols.EditTheme.spinner

  // ===================================================================================================================

  def autosizeTextareaProps(mode    : Mode,
                            position: Option[PreviewFeature.Position],
                            enabled : Enabled,
                            validity: Validity,
                            value   : String,
                            tagMod  : TagMod,
                            font    : Font = Font.Default): TagMod =
    TagMod(
      *.textEditor((validity, position, mode, font)),
      ^.value := value,
      ^.disabled := enabled.is(Disabled),
      tagMod)

  // ===================================================================================================================

  private[this] lazy val editableInline: TagMod =
    TagMod(
      *.inlineEdit,
      ^.title := UiText.doubleClickToEdit)

  def editableInline(startEdit: Callback): TagMod =
    TagMod(editableInline, ^.onDblClick --> startEdit)

  def editableInline(startEdit: Option[Callback]): TagMod =
    startEdit.fold(TagMod.empty)(editableInline(_))

  // ===================================================================================================================

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

  // ===================================================================================================================

  /** no preview, fullscreen, or font */
  def renderEditor(status      : EditorStatus,
                   editor      : Validity => VdomElement,
                   readOnlyView: => VdomNode,
                   instructions: => TagMod): VdomNode =
    renderEditor(
      status          = status,
      editor          = (_, v) => editor(v),
      readOnlyView    = readOnlyView,
      instructions    = instructions,
      style           = Style.default,
      previewRW       = PreviewFeature.ReadWrite.Single.neverShow,
      previewWantOpen = false,
      previewBody     = EmptyVdom,
    )

  /** no fullscreen or font */
  def renderEditor(status         : EditorStatus,
                   editor         : (Enabled, Validity) => VdomElement,
                   readOnlyView   : => VdomNode,
                   instructions   : => TagMod,
                   style          : Style,
                   previewRW      : PreviewFeature.ReadWrite.Single,
                   previewWantOpen: => Boolean,
                   previewBody    : => VdomNode): VdomNode =
    renderEditor(
      status             = status,
      optionalFullscreen = None,
      editor             = (_, e, _, v) => editor(e, v),
      readOnlyView       = readOnlyView,
      instructions       = _ => instructions,
      style              = style,
      font               = Font.Default,
      previewRW          = previewRW,
      previewWantOpen    = previewWantOpen,
      previewBody        = previewBody,
    )

  /** full */
  def renderEditor(status            : EditorStatus,
                   optionalFullscreen: Option[OptionalFullscreen],
                   editor            : (Layout, Enabled, Option[OptionalFullscreen.Ctx], Validity) => VdomElement,
                   readOnlyView      : => VdomNode,
                   instructions      : Option[OptionalFullscreen.Ctx] => TagMod,
                   style             : Style,
                   font              : Font,
                   previewRW         : PreviewFeature.ReadWrite.Single,
                   previewWantOpen   : => Boolean,
                   previewBody       : => VdomNode): VdomNode =
    editcontrols.EditTheme.renderEditor(
      status             = status,
      optionalFullscreen = optionalFullscreen,
      editor             = editor,
      readOnlyView       = readOnlyView,
      instructions       = instructions,
      style              = style,
      font               = font,
      previewRW          = previewRW,
      previewWantOpen    = previewWantOpen,
      previewBody        = previewBody,
    )
}
