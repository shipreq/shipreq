package shipreq.webapp.base.ui

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
      BaseStyles.textEditor((validity, style)),
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

  sealed trait Style
  object Style {
    case object OptionalPreviewUnderText extends Style
    case object PreviewUnderText         extends Style
    case object PreviewOnRightOfText     extends Style

    implicit def univEq: UnivEq[Style] =
      UnivEq.derive

    implicit def reusability: Reusability[Style] =
      Reusability.by_==
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
      style        = Style.OptionalPreviewUnderText,
      preview      = EmptyVdom,
    )

  def renderEditor(status      : EditorStatus,
                   editor      : Validity => VdomElement,
                   readOnlyView: => VdomNode,
                   instructions: => TagMod,
                   style       : Style,
                   preview     : => TagMod): VdomTag = {

    status match {
      case EditorStatus.Ignore | EditorStatus.Valid(_) =>
        style match {

          case Style.OptionalPreviewUnderText | Style.PreviewUnderText =>
            <.div(
              editor(Valid),
              instructions,
              preview)

          case Style.PreviewOnRightOfText =>
            <.div(*.textEditorLeftPreviewRight,
              <.div(editor(Valid), instructions),
              <.div(preview))
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
        <.div(*.textEditor((*.EditorState.InTransit, style)),
          <.div(spinner),
          <.div(*.textEditorInTransitValue, readOnlyView))
    }
  }

  def renderPreview(p       : => PreviewFeature.ReadWrite.Single,
                    style   : Style,
                    wantOpen: => Boolean,
                    view    : => VdomNode): VdomNode = {
    def render =
      <.div(*.richTextPreview(style),
        <.div(*.richTextPreviewHeader, "Preview"),
        <.div(*.richTextPreviewBodyOuter,
          <.div(*.richTextPreviewBodyInner(style), view)))

    style match {
      case Style.OptionalPreviewUnderText => p.reactCollapse(wantOpen)(render)
      case Style.PreviewUnderText
         | Style.PreviewOnRightOfText     => render
    }
  }
}
