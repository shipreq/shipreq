package shipreq.webapp.member.feature

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.feature.EditorStatus
import shipreq.webapp.member.UiText
import shipreq.webapp.member.ui.{BaseStyles => *, _}

object EditControlsFeature {

  type ExtraControls = shipreq.webapp.member.feature.editcontrols.ExtraControls
  val  ExtraControls = shipreq.webapp.member.feature.editcontrols.ExtraControls

  val  Controls      = shipreq.webapp.member.feature.editcontrols.Controls

  type Font          = shipreq.webapp.member.feature.editcontrols.Font
  val  Font          = shipreq.webapp.member.feature.editcontrols.Font

  val  Keys          = shipreq.webapp.member.feature.editcontrols.Keys

  type Layout        = shipreq.webapp.member.feature.editcontrols.Layout
  val  Layout        = shipreq.webapp.member.feature.editcontrols.Layout

  type Mode          = shipreq.webapp.member.feature.editcontrols.Mode
  val  Mode          = shipreq.webapp.member.feature.editcontrols.Mode

  type OpenPreview   = shipreq.webapp.member.feature.editcontrols.OpenPreview
  val  OpenPreview   = shipreq.webapp.member.feature.editcontrols.OpenPreview

  type Style         = shipreq.webapp.member.feature.editcontrols.Style
  val  Style         = shipreq.webapp.member.feature.editcontrols.Style

  type WhenInTransit = shipreq.webapp.member.feature.editcontrols.WhenInTransit
  val  WhenInTransit = shipreq.webapp.member.feature.editcontrols.WhenInTransit

  final val defaultAbortVerb  = editcontrols.Instructions.defaultAbortVerb
  final val defaultCommitVerb = editcontrols.Instructions.defaultCommitVerb

  def spinner = shipreq.webapp.member.feature.editcontrols.EditTheme.spinner

  // ===================================================================================================================

  def autosizeTextareaProps(mode    : Mode,
                            position: Option[PreviewFeature.Position],
                            enabled : Enabled,
                            validity: Validity,
                            value   : String,
                            tagMod  : TagMod,
                            font    : Font = Font.Default): TagMod =
    TagMod(
      *.textEditor((validity, position, mode, font, enabled)),
      ^.value := value,
      ^.readOnly := enabled.is(Disabled), // readOnly instead of disabled so that editor doesn't lose focus
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

  def onTextareaEditorMount(ref: Ref.ToScalaComponent[_, _, _], autoFocus: Boolean): Callback =
    onTextareaEditorMount(ref).when_(autoFocus)

  def onTextareaEditorMount(ref: Ref.ToScalaComponent[_, _, _]): Callback =
    ref.get.flatMap(m => onTextareaEditorMount(m.getDOMNode).toCBO)

  def onTextareaEditorMount(cd: ComponentDom): Callback =
    Callback.traverseOption(cd.toHtml)(h => Callback {
      val t = h.domCast[html.TextArea]
      val l = t.value.length
      t.setSelectionRange(l, l)
    })

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
                   previewBody    : => TagMod): VdomNode =
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
                   previewBody       : => TagMod): VdomNode =
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
