package shipreq.webapp.base.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.UiText
import shipreq.webapp.base.feature.{EditorStatus, PreviewFeature}
import shipreq.webapp.base.ui.semantic.Icon
import shipreq.webapp.base.ui.{BaseStyles => *}

object EditTheme {

  // This probably shouldn't be exposed
  private[this] val editableInline: TagMod =
    TagMod(
      *.inlineEdit,
      ^.title := UiText.doubleClickToEdit)

  def editableInline(startEdit: Callback): TagMod =
    editableInline(^.onDblClick --> startEdit)

  def editableInline(startEdit: Option[Callback]): TagMod =
    startEdit.fold(TagMod.empty)(editableInline(_))

  def autosizeTextareaProps(validity: Validity, value: String, tagMod: TagMod): TagMod =
    TagMod(
      BaseStyles.textEditor(validity),
      ^.value := value,
      tagMod)

  val spinner: VdomTag =
    Icon.CircleNotched.loading.tag(^.marginRight := "0")

  def renderEditor(status      : EditorStatus,
                   editor      : Validity => VdomElement,
                   readOnlyView: => VdomNode,
                   instructions: => TagMod,
                   preview     : => TagMod = EmptyVdom): VdomTag = {

    status match {
      case EditorStatus.Ignore | EditorStatus.Valid(_) =>
        <.div(
          editor(Valid),
          instructions,
          preview)

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
        <.div(*.textEditor(*.EditorState.InTransit),
          <.div(spinner),
          <.div(*.textEditorInTransitValue, readOnlyView))
    }
  }

  def renderPreview(p: PreviewFeature.ReadWrite.Single, wantOpen: => Boolean, view: => VdomNode): VdomNode =
    p.reactCollapse(wantOpen)(
      <.div(*.richTextPreview,
        <.div(*.richTextPreviewHeader, "Preview"),
        <.div(*.richTextPreviewBodyOuter,
          <.div(*.richTextPreviewBodyInner, view))))
}
