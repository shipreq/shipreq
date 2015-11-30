package shipreq.webapp.client.app.ui.newui

import japgolly.scalajs.jquery.TextComplete
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom
import scalacss.ScalaCssReact._
import shipreq.base.util.ScalaExt._
import shipreq.webapp.base.data._
import shipreq.webapp.base.text._
import shipreq.webapp.client.app.ui.ProjectWidgets
import shipreq.webapp.client.app.ui.Style.{reqtable => *} // TODO Not anymore
import shipreq.webapp.client.app.ui.reqtable.edit.AutoComplete
import shipreq.webapp.client.data.DataReusability._
import shipreq.webapp.client.lib.ui.KeyHandlers
import shipreq.webapp.client.lib.ui.feature._
import shipreq.webapp.client.lib.{Contextualise, HideDead}

sealed abstract class RichTextEditor[TextType <: Text.Generic](name: String, final val text: TextType) {
  private def supportsPTM     = text match { case _: Atom.PlainTextMarkup => true; case _ => false }
  private def supportsReqRefs = text match { case _: Atom.ReqRef          => true; case _ => false }
  private def supportsTags    = text match { case _: Atom.TagRef          => true; case _ => false }
  private def supportsIssues  = text match { case _: Atom.Issue           => true; case _ => false }

  def mkAutoComplete(p: Project, pt: PlainText.ForProject, ts: TextSearch): AutoCompleteFeature.ForChild = {
    var ac = Vector.empty[TextComplete.Strategy]

    if (supportsIssues || supportsTags)
      ac :+= AutoComplete.hashtag(p, HideDead, issues = supportsIssues, tags = supportsTags)(Contextualise)

    if (supportsReqRefs) {
      ac :+= AutoComplete.reqCode.ref(p, pt)
      ac :+= AutoComplete.req(ts, AutoComplete.reqItems(p, pt), Contextualise)
    }

    if (supportsPTM)
      ac :+= AutoComplete.math

    AutoCompleteFeature.Strategies(ac: _*)
  }

  case class Props(project       : Project,
                   plainText     : PlainText.ForProject,
                   textSearch    : TextSearch,
                   projectWidgets: ProjectWidgets,
                   edit          : ExternalVar[String],
                   commit        : text.OptionalText => Callback,
                   keyHandlers   : KeyHandlers)

  private val editorRef = Ref[dom.html.TextArea]("i")

  // This is an editor - you can't edit Dead stuff. Assume all content is Live.
  @inline private def hardcodedLive = Live

  val liveCorrect: EndoFn[String] =
    if (text.singleLine)
      RichTextEditor.correctSingleLineText
    else
      identity

  class Backend($: BackendScope[Props, Unit]) {
    private val pxProject    = Px.bs($).propsA(_.project)
    private val pxPlainText  = Px.bs($).propsA(_.plainText)
    private val pxTextSearch = Px.bs($).propsA(_.textSearch)

    val pxAutoComplete =
      Px.apply3(pxProject, pxPlainText, pxTextSearch)(mkAutoComplete)

    val updateState: ReactEventTA => Callback =
      e => $.props >>= (_.edit.set(liveCorrect(e.target.value)))

    def render(p: Props) = {
      val richText = text.parse(p.project)(p.edit.value)
      val validated = EditValidationFeature(Validators.genericRichText(p.plainText, richText))

      def keyHandlers: KeyHandlers =
        p.keyHandlers + validated.commitByKeyboard(p.commit, text.singleLine)

      def editor =
        <.textarea(
          *.cellEditor(validated.validity),
          keyHandlers,
          ^.ref := editorRef,
          ^.value := p.edit.value,
          ^.onChange ==> updateState)

      <.div(
        editor,
        validated.renderFailure,
        "Preview", <.div(*.textEditPreview, p.projectWidgets.format(hardcodedLive, richText)))
    }
  }

  val component =
    ReactComponentB[Props]("TagEditor")
      .renderBackend[Backend]
      // TODO .configure(Reusability.shouldComponentUpdate)
      .configure(AutoCompleteFeature.installBP(editorRef, _.pxAutoComplete.value(), _.edit.set))
      .build
}

// ===================================================================================================================

object RichTextEditor {
  val correctSingleLineText: EndoFn[String] = {
    val r = "[\\r\\n]+".r
    r.replaceAllIn(_, " ")
  }

  object GenericReqTitle extends RichTextEditor("GRT", Text.GenericReqTitle)

  object ReqCodeGroupTitle extends RichTextEditor("RCGT", Text.ReqCodeGroupTitle)

  object DeletionReason extends RichTextEditor("DR", Text.DeletionReason)

  final class CustomTextField(fid: CustomField.Text.Id) extends RichTextEditor("CTF", Text.CustomTextField)
}
