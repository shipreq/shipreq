package shipreq.webapp.client.app.ui.reqtable
package edit

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.jquery.{TextComplete => TC}
import scalacss.ScalaCssReact._
import org.scalajs.dom.raw.HTMLTextAreaElement
import scalajs.js
import shipreq.base.util.ScalaExt._
import shipreq.base.util.Validity
import shipreq.webapp.base.data._
import shipreq.webapp.base.protocol.UpdateContentCmd
import shipreq.webapp.base.text._
import shipreq.webapp.client.app.ui.{VUCA, RemoteDataEditor, ProjectWidgets}
import shipreq.webapp.client.app.ui.Style.{reqtable => *}
import shipreq.webapp.client.lib.{TCB, HideDead, Contextualise}
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

    def prepare(project       : Px[Project],
                projectText   : Px[PlainText.ForProject],
                projectWidgets: Px[ProjectWidgets],
                textSearch    : Px[TextSearch]): VUCA[String, t.OptionalText] => Props = {

      val autoComplete = mkAutoComplete(project, projectText, textSearch)

      Props(_, project.value(), projectText.value(), projectWidgets.value(), autoComplete.value())
    }

    def selfManaged(initial       : t.OptionalText,
                    project       : Px[Project],
                    projectText   : Px[PlainText.ForProject],
                    projectWidgets: Px[ProjectWidgets],
                    textSearch    : Px[TextSearch],
                    setSelf       : RemoteDataEditor.SetOpStateFor[String],
                    commitFn      : t.OptionalText => RemoteDataEditor.OnCommit): RemoteDataEditor.StateFor[String] = {

      def init = projectText.value() format initial

      val props = prepare(project, projectText, projectWidgets, textSearch)

      val onCommit = RemoteDataEditor.CommitFilter(commitFn).ignoreIfEqual(initial)

      RemoteDataEditor.default[String, String](
        init, identity, setSelf,
        (s, u, a, commit) => props(VUCA(s, u, v => commit(onCommit(v)), a)).render)
    }

    type SubjectId
    def mkUpdateContentCmd: (SubjectId, t.OptionalText) => UpdateContentCmd

    def edit(subjectId     : SubjectId,
             initial       : t.OptionalText,
             project       : Px[Project],
             projectText   : Px[PlainText.ForProject],
             projectWidgets: Px[ProjectWidgets],
             textSearch    : Px[TextSearch],
             setSelf       : RemoteDataEditor.SetOpStateFor[String],
             commitFn      : UpdateContentOnCommit): RemoteDataEditor.StateFor[String] = {
      val onCommit = commitFn.cmap[t.OptionalText](mkUpdateContentCmd(subjectId, _))
      selfManaged(initial, project, projectText, projectWidgets, textSearch, setSelf, onCommit)
    }

    // -----------------------------------------------------------------------------------------------------------------

    case class Props(vuca          : VUCA[String, t.OptionalText],
                     project       : Project,
                     projectText   : PlainText.ForProject,
                     projectWidgets: ProjectWidgets,
                     autoComplete  : AutoComplete)  {

      val parseResult = {
        val txt = t.parse(project)(vuca.value)
        Validators.genericRichText(projectText, txt).disjunction
      }

      def render = component(this)
    }

    val component =
      ReactComponentB[Props](name)
        .stateless
        .backend(new Backend(_))
        .render(_.backend.render)
        .configure(UI.installTextCompleteP(textEditorRef, _.autoComplete, _.vuca.update))
        .build

    val correctOnChange: EndoFn[String] =
      if (t.singleLine) correctSingleLineText else identity

    class Backend($: BackendScope[Props, Unit]) {

      val updateState: ReactEventI => Callback =
        e => $.props.vuca.update(correctOnChange(e.target.value))

      def render: ReactElement = {
        val p = $.props
        val parseResult = p.parseResult

        val keyHandlers =
          KeyHandlers.commitAndAbortD(p.vuca.abort, parseResult, p.vuca.commit, t.singleLine)

        val editor =
          <.textarea(
            *.cellEditor(Validity(parseResult)),
            keyHandlers,
            ^.ref       := textEditorRef,
            ^.value     := p.vuca.value,
            ^.onChange ==> updateState)

        parseResult.fold(
          e => <.div(editor, <.div(cellErrorMsgStyle, e.toText)),
          v => <.div(editor, "Preview", <.div(*.textEditPreview, p.projectWidgets format v)))
      }
    }
  }

  // ===================================================================================================================

  object GenericReqTitle extends Base("GenericReqDesc editor", Text.GenericReqTitle) {
    override type SubjectId = GenericReqId
    override def mkUpdateContentCmd = SetGenericReqTitle
  }

  object ReqCodeGroupTitle extends Base("ReqCodeGroupTitle editor", Text.ReqCodeGroupTitle) {
    override type SubjectId = ReqCodeId
    override def mkUpdateContentCmd = SetReqCodeGroupTitle
  }

  class CustomTextField(fid: CustomField.Text.Id) extends Base("CustomTextField editor", Text.CustomTextField) {
    override type SubjectId = ReqId
    override def mkUpdateContentCmd = SetCustomTextField(_, fid, _)
  }
}
