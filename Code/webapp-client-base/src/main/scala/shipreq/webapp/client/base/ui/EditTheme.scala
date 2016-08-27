package shipreq.webapp.client.base.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.UiText
import shipreq.webapp.client.base.feature.EditorStatus
import shipreq.webapp.client.base.ui.semantic.Icon
import shipreq.webapp.client.base.ui.{BaseStyles => *}

object EditTheme {

  // This probably shouldn't be exposed
  private[this] val editableInline: TagMod =
    TagMod(
      *.inlineEdit,
      ^.title := UiText.doubleClickToEdit)

  def editableInline(startEdit: Callback): TagMod =
    editableInline + (^.onDblClick --> startEdit)

  def editableInline(startEdit: Option[Callback]): TagMod =
    startEdit.fold(EmptyTag)(editableInline(_))

  def autosizeTextarea(ref: String, validity: Validity, value: String, tagMod: TagMod): ReactElement =
    AutosizeTextarea.withRef(ref)(
      BaseStyles.textEditor(validity),
      ^.value := value,
      tagMod)

  def renderEditor(status      : EditorStatus,
                   editor      : Validity => ReactElement,
                   readOnlyView: => ReactNode,
                   instructions: => TagMod,
                   preview     : => TagMod = EmptyTag): ReactTag = {

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
          <.div(Icon.CircleNotched.loading.tag),
          <.div(*.textEditorInTransitValue, readOnlyView))
    }
  }

}
