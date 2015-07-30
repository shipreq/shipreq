package shipreq.webapp.client.app.ui.reqtable
package edit

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.jquery.{TextComplete => TC}
import scalacss.ScalaCssReact._
import org.scalajs.dom.raw.HTMLTextAreaElement
import scalajs.js
import scalaz.effect.IO
import shipreq.base.util.ScalaExt._
import shipreq.base.util.Validity
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.UpdateContentCmd
import shipreq.webapp.base.text._
import shipreq.webapp.client.app.ui.{RemoteDataEditor, ProjectWidgets}
import shipreq.webapp.client.app.ui.Style.{reqtable => *}
import shipreq.webapp.client.lib.{TIO, HideDead, Contextualise}
import shipreq.webapp.client.lib.ui.{KeyHandlers, UI}
import Text.Equality._
import UpdateContentCmd._

object RichTextEditor {

  type AutoComplete = ReusableVal[TC.Strategies]

  private val correctSingleLineText: EndoFn[String] = {
    val r = "[\\r\\n]+".r
    r.replaceAllIn(_, " ")
  }

  private val textEditorRef = Ref[HTMLTextAreaElement]("i")

  // ===================================================================================================================
  sealed abstract class Base[TextType <: Text.Generic](name: String, final val t: TextType) {

    def supportsPTM     = t match { case _: Atom.PlainTextMarkup => true; case _ => false }
    def supportsReqRefs = t match { case _: Atom.ReqRef          => true; case _ => false }
    def supportsTags    = t match { case _: Atom.TagRef          => true; case _ => false }
    def supportsIssues  = t match { case _: Atom.Issue           => true; case _ => false }

    def mkAutoComplete(project: Px[Project], projectText: Px[PlainText.ForProject], textSearch: Px[TextSearch]): Px[AutoComplete] = {
      @inline def $ = AutoComplete

      for {
        p <- project
        t <- projectText
        s <- textSearch
      } yield ReusableVal.byRef {
        var ac: TC.Strategies = new js.Array

        if (supportsIssues || supportsTags)
          ac.push($.hashtag(p, HideDead, issues = supportsIssues, tags = supportsTags)(Contextualise))

        if (supportsReqRefs)
          ac.push(
            $.reqCode.ref(p, t),
            $.req(s, $.reqItems(p, t), Contextualise))

        if (supportsPTM)
          ac push $.math

        ac
      }
    }

    type SubjectId

    def mkChange: (SubjectId, t.OptionalText) => UpdateContentCmd

    def apply(initial       : t.OptionalText,
              subjectId     : SubjectId,
              project       : Px[Project],
              projectText   : Px[PlainText.ForProject],
              projectWidgets: Px[ProjectWidgets],
              textSearch    : Px[TextSearch])
             (setSelf       : RemoteDataEditor.SetOpState,
              onCommit0     : UpdateContentOnCommit): Cell.State = {

      def init: String =
        projectText.value() format initial

      val onCommit = onCommit0.cmapToInitial(initial)(mkChange(subjectId, _))

      val autoComplete = mkAutoComplete(project, projectText, textSearch)

      RemoteDataEditor.opDefault[String, String](
        init, identity, setSelf,
        (s, u, abort, commit) =>
          Props(s, u, abort, v => commit(onCommit(v)), project, projectText, projectWidgets, autoComplete.value()).apply)
    }

    // -----------------------------------------------------------------------------------------------------------------

    case class Props(state         : String,
                     stateUpdate   : String => IO[Unit],
                     abort         : TIO.Abort,
                     commit        : t.OptionalText => TIO.Commit,
                     project       : Px[Project],
                     projectText   : Px[PlainText.ForProject],
                     projectWidgets: Px[ProjectWidgets],
                     autoComplete  : AutoComplete)  {

      def apply = component(this)
    }

    val component =
      ReactComponentB[Props](name)
        .stateless
        .backend(new Backend(_))
        .render(_.backend.render)
        .configure(UI.installTextCompleteP(textEditorRef, _.autoComplete, _.stateUpdate))
        .build

    val correctOnChange: EndoFn[String] =
      if (t.singleLine) correctSingleLineText else identity

    class Backend($: BackendScope[Props, Unit]) {

      val updateState: ReactEventI => IO[Unit] =
        e => $.props.stateUpdate(correctOnChange(e.target.value))

      def render: ReactElement = {
        val p = $.props

        val parseResult = {
          val txt = t.parse(p.project.value())(p.state)
          Validators.genericRichText(p.projectText.value(), txt).disjunction
        }

        val keyHandlers =
          KeyHandlers.commitAndAbortD(p.abort, parseResult, p.commit, t.singleLine)

        val editor =
          <.textarea(
            *.cellEditor(Validity(parseResult)),
            keyHandlers,
            ^.ref       := textEditorRef,
            ^.value     := p.state,
            ^.onChange ~~> updateState)

        parseResult.fold(
          e => <.div(editor, <.div(cellErrorMsgStyle, e.toText)),
          v => <.div(editor, "Preview", <.div(*.textEditPreview, p.projectWidgets.value() format v)))
      }
    }
  }

  // ===================================================================================================================

  object GenericReqTitle extends Base("GenericReqDesc editor", Text.GenericReqTitle) {
    override type SubjectId = GenericReqId
    override def mkChange = SetGenericReqTitle
  }

  object ReqCodeGroupTitle extends Base("ReqCodeGroupTitle editor", Text.ReqCodeGroupTitle) {
    override type SubjectId = ReqCodeId
    override def mkChange = SetReqCodeGroupTitle
  }

  class CustomTextField(fid: CustomField.Text.Id) extends Base("CustomTextField editor", Text.CustomTextField) {
    override type SubjectId = ReqId
    override def mkChange = SetCustomTextField(_, fid, _)
  }
}
