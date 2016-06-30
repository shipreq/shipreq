package shipreq.webapp.client.project.widgets.high

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text.Text.Equality._
import shipreq.webapp.base.text._
import shipreq.webapp.client.base.feature.EditorStatus
import shipreq.webapp.client.base.lib.KeyboardTheme
import shipreq.webapp.client.base.ui.AutosizeTextarea
import shipreq.webapp.client.base.ui.semantic.{Colour, Icon, Label}
import shipreq.webapp.client.project.app.Style.{reqtable => *}
import shipreq.webapp.client.project.feature._
import shipreq.webapp.client.project.lib.AutoComplete
import shipreq.webapp.client.project.lib.DataReusability._
import RichTextEditor2.hardcodedLive

sealed abstract class RichTextEditor2[TextType <: Text.Generic](name: String, final val text: TextType) {

  type Commit = text.OptionalText ~=> Callback

  case class Props(project       : Project,
                   plainText     : PlainText.ForProject,
                   textSearch    : TextSearch,
                   projectWidgets: ProjectWidgets,
                   edit          : ReusableVar[String],
                   asyncStatus   : Option[EditorStatus.Async],
                   abort         : Callback,
                   commit        : Commit,
                   preview       : PreviewFeature.ForChild,
                   preEditValue  : Option[text.OptionalText]) {

    val richText    = text.parse(project)(edit.value)
    val parseResult = Validators.genericRichText(plainText, richText)
    val validated   = EditValidationFeature.compareOption(parseResult)(preEditValue)
    val status      = asyncStatus getOrElse EditorStatus.validationResult(parseResult)(commit)
    def showPreview = validated.value.isChanged

    def render = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.never // TODO Reusability.caseClass

  private val editorRef = Ref.to(AutosizeTextarea.Component, "i")

  private val errorPointingUp = Label.Style(Label.Type.PointingUp, Colour.Red).div

  val liveCorrect: EndoFn[String] =
    RichTextEditor2.liveCorrect(text)

  final class Backend($: BackendScope[Props, Unit]) {
    private val pxProject    = Px.bs($).propsA(_.project)
    private val pxPlainText  = Px.bs($).propsA(_.plainText)
    private val pxTextSearch = Px.bs($).propsA(_.textSearch)

    val pxAutoComplete =
      Px.apply3(pxProject, pxPlainText, pxTextSearch)(AutoComplete.forRichText(text))

    val textareaConst: TagMod = {
      val minRows = text.lineCardinality match {
        case SingleLine => 1
        case MultiLine  => 3
      }

      val keys =
        KeyboardTheme.abortCriterion.handle($.props.flatMap(_.abort)) +
        KeyboardTheme.commitCO($.props.map(_.status.getCommit), text.lineCardinality)

      val updateState: ReactEventTA => Callback =
        e => $.props >>= (p =>
          p.status.wrapEdit(p.edit.set(liveCorrect(e.target.value)) >> p.preview.onEdit))

      TagMod(
        ^.autoFocus := true,
        ^.rows      := minRows,
        ^.onChange ==> updateState,
        ^.onBlur   --> $.props.flatMap(_.preview.onBlur),
        ^.onFocus  --> $.props.flatMap(_.preview.onFocus),
        keys)
    }

    def getTextarea() =
      editorRef($).get.getDOMNode()

    def render(p: Props) = {

      def editor(extra: TagMod): ReactElement =
        AutosizeTextarea.withRef(editorRef)(
          *.cellEditor(p.validated.validity),
          ^.value := p.edit.value,
          textareaConst,
          extra)

      def instructions =
        KeyboardTheme.instructionsForCommitAbort(p.status.getCommit, p.abort, text.lineCardinality)(
          ^.textAlign.right
        )

      def richText =
        p.projectWidgets.format(hardcodedLive, p.richText)

      def preview =
        p.preview.reactCollapse(p.showPreview)(
          <.div(
            ^.ref := "p",
            "Preview",
            <.div(*.textEditPreview, richText)))

      p.status match {
        case EditorStatus.Ignore | EditorStatus.Valid(_) =>
          <.div(
            editor(EmptyTag),
            instructions,
            preview)

        case EditorStatus.Invalid(err) =>
          <.div(
            editor(EmptyTag), // TODO add error background
            errorPointingUp(err),
            preview)

        case EditorStatus.AsyncError(err, _, _) =>
          <.div(
            editor(EmptyTag),
            errorPointingUp(err),
            preview)

        case EditorStatus.InTransit =>
//            %div.texteditor.disabled
//              %div
//                %i.icon.loading.circle.notched
//              %div.value
//                richText
          <.div(
            <.div(Icon.CircleNotched.loading.tag),
            <.div(richText))
      }

    }
  }

  val Component =
    ReactComponentB[Props]("RichTextEditor2:" + name)
      .renderBackend[Backend]
      .configure(
        Reusability.shouldComponentUpdate,
        AutoCompleteFeature.install(
          _.backend.getTextarea(),
          (p, b) => b.pxAutoComplete.value(),
          (p, b) => p.edit.set))
      .build
}

// ===================================================================================================================

object RichTextEditor2 {
  val correctSingleLineText: EndoFn[String] = {
    val r = "[\\r\\n]+".r
    r.replaceAllIn(_, "")
  }

  def liveCorrect(text: Text.Generic): EndoFn[String] =
    text.lineCardinality match {
      case SingleLine => RichTextEditor2.correctSingleLineText
      case MultiLine  => identity
    }

  // This is an editor - you can't edit Dead stuff. Assume all content is Live.
  @inline def hardcodedLive = Live

  object GenericReqTitle extends RichTextEditor2("GRT", Text.GenericReqTitle)

  object UseCaseTitle extends RichTextEditor2("UCT", Text.UseCaseTitle)

  object ReqCodeGroupTitle extends RichTextEditor2("RCGT", Text.ReqCodeGroupTitle)

  object CustomTextField extends RichTextEditor2("CTF", Text.CustomTextField)

  object DeletionReason extends RichTextEditor2("DR", Text.DeletionReason)
}
