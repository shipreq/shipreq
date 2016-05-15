package shipreq.webapp.client.project.widgets.high

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom
import scalacss.ScalaCssReact._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.univeq._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text._
import shipreq.webapp.base.validation.ValidUpdateVR
import shipreq.webapp.client.project.app.Style.{reqtable => *} // TODO Not anymore
import shipreq.webapp.client.project.lib.AutoComplete
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.feature._
import RichTextEditor.hardcodedLive
import Text.Equality._

sealed abstract class RichTextEditor[TextType <: Text.Generic](name: String, final val text: TextType) {

  /** Extra properties to apply to the tag. */
  type Extra = ValidUpdateVR[text.OptionalText] ~=> TagMod

  val noExtra: Extra =
    ReusableFn(_ => EmptyTag)

  case class Props(project       : Project,
                   plainText     : PlainText.ForProject,
                   textSearch    : TextSearch,
                   projectWidgets: ProjectWidgets,
                   edit          : ReusableVar[String],
                   preview       : PreviewFeature.ForChild,
                   preEditValue  : Option[text.OptionalText],
                   extra         : Extra) {

    val richText    = text.parse(project)(edit.value)
    val parseResult = Validators.genericRichText(plainText, richText)
    val validated   = EditValidationFeature.compareOption(parseResult)(preEditValue)
    def showPreview = validated.value.isChanged

    def render = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.caseClass

  private val editorRef = Ref[dom.html.TextArea]("i")

  val liveCorrect: EndoFn[String] =
    RichTextEditor.liveCorrect(text)

  final class Backend($: BackendScope[Props, Unit]) {
    private val pxProject    = Px.bs($).propsA(_.project)
    private val pxPlainText  = Px.bs($).propsA(_.plainText)
    private val pxTextSearch = Px.bs($).propsA(_.textSearch)

    val pxAutoComplete =
      Px.apply3(pxProject, pxPlainText, pxTextSearch)(AutoComplete.forRichText(text))

    val updateState: ReactEventTA => Callback =
      e => $.props >>= (p =>
        p.edit.set(liveCorrect(e.target.value)) >> p.preview.onEdit)

    def render(p: Props) = {
      def editor =
        <.textarea(
          *.cellEditor(p.validated.validity),
          p.extra(p.validated.value),
          ^.ref       := editorRef,
          ^.value     := p.edit.value,
          ^.onBlur   --> p.preview.onBlur,
          ^.onFocus  --> p.preview.onFocus,
          ^.onChange ==> updateState)

      def preview =
        p.preview.reactCollapse(p.showPreview)(
          <.div(
            ^.ref := "p",
            "Preview",
            <.div(*.textEditPreview, p.projectWidgets.format(hardcodedLive, p.richText))))

      <.div(
        editor,
        p.validated.renderFailure,
        preview)
    }
  }

  val Component =
    ReactComponentB[Props]("RichTextEditor:" + name)
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .configure(AutoCompleteFeature.installBP(editorRef, _.pxAutoComplete.value(), _.edit.set))
      .build
}

// ===================================================================================================================

object RichTextEditor {
  val correctSingleLineText: EndoFn[String] = {
    val r = "[\\r\\n]+".r
    r.replaceAllIn(_, " ")
  }

  def liveCorrect(text: Text.Generic): EndoFn[String] =
    text.lineCardinality match {
      case SingleLine => RichTextEditor.correctSingleLineText
      case MultiLine  => identity
    }

  // This is an editor - you can't edit Dead stuff. Assume all content is Live.
  @inline def hardcodedLive = Live

  object GenericReqTitle extends RichTextEditor("GRT", Text.GenericReqTitle)

  object UseCaseTitle extends RichTextEditor("UCT", Text.UseCaseTitle)

  object ReqCodeGroupTitle extends RichTextEditor("RCGT", Text.ReqCodeGroupTitle)

  object CustomTextField extends RichTextEditor("CTF", Text.CustomTextField)

  object DeletionReason extends RichTextEditor("DR", Text.DeletionReason)
}
