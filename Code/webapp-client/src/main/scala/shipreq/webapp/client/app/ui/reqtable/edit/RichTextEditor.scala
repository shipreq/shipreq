package shipreq.webapp.client.app.ui.reqtable
package edit

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.jquery.{TextComplete => TC}
import shipreq.webapp.client.util.IsOK
import scalacss.ScalaCssReact._
import org.scalajs.dom.ext.KeyValue
import org.scalajs.dom.raw.HTMLTextAreaElement
import shipreq.webapp.client.app.ui.ProjectWidgets
import scalaz.effect.IO
import shipreq.webapp.base.data._
import shipreq.webapp.base.text._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.{Must, UnivEq, Px}
import shipreq.base.util.effect.IoUtils, IoUtils.IoExt
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.client.app.ui.Style.{reqtable => *}
import shipreq.webapp.client.lib.ui.{KeyHandlers, UI}

// TODO Limit size

object RichTextEditor {

  type AutoComplete = Px[TC.Strategies]
  type S = String

  private val correctSingleLineText: EndoFn[String] = {
    val r = "[\\r\\n]+".r
    r.replaceAllIn(_, " ")
  }

  private val textEditorRef = Ref[HTMLTextAreaElement]("i")

  // ===================================================================================================================
  sealed abstract class Base[TextType <: Text.Generic](name: String, final val t: TextType) {

    def apply(initial       : t.OptionalText,
              project       : Px[Project],
              projectText   : Px[PlainText.ForProject],
              projectWidgets: Px[ProjectWidgets],
              textSearch    : Px[TextSearch],
              setState      : Option[Cell.State] => IO[Unit]): Cell.State = {

      def init: S =
        projectText.value() format initial

      val abort: IO[Unit] =
        setState(None)

      val commit: t.OptionalText => IO[Unit] =
      // TODO If change occurred, send to server & lock cell. (If unchanged, clear state.)
        s => setState(None) >>> IO{ println("Sent to ze server: " + s) }

      val autoComplete = mkAutoComplete(project, projectText, textSearch)

      Cell.selfManage(setState, init)(
        Props(_, _, abort, commit, project, projectWidgets, autoComplete).apply)
    }

    def supportsTags   = t match { case _: Atom.TagRef => true; case _ => false }
    def supportsIssues = t match { case _: Atom.Issue  => true; case _ => false }

    def mkAutoComplete(project: Px[Project], projectText: Px[PlainText.ForProject], textSearch: Px[TextSearch]): AutoComplete = {
      @inline def legalIf[A](guard: Boolean, s: => Stream[A]): Stream[A] =
        if (guard) s else Stream.empty
      for {
        p <- project
        t <- projectText
        s <- textSearch
      } yield
        TC.Strategies(
          AutoComplete.hashtag(
            legalIf(supportsIssues, p.customIssueTypes.data.values.toStream),
            legalIf(supportsTags  , p.tags.data.vstream(_.tag).filterT[ApplicableTag]),
            prefix = true),
          AutoComplete.req(s, AutoComplete.reqItems(p, t), prefix = true),
          AutoComplete.math
        )
    }

    // -----------------------------------------------------------------------------------------------------------------

    case class Props(state         : S,
                     stateUpdate   : S => IO[Unit],
                     abort         : IO[Unit],
                     commit        : t.OptionalText => IO[Unit],
                     project       : Px[Project],
                     projectWidgets: Px[ProjectWidgets],
                     autoComplete  : AutoComplete)  {

      def apply = component(this)
    }

    val component =
      ReactComponentB[Props](name)
        .stateless
        .backend(new Backend(_))
        .render(_.backend.render)
        .configure(UI.installTextCompleteR(textEditorRef, _.props.autoComplete, _.props.stateUpdate))
        .build

    val correctOnChange: EndoFn[String] =
      if (t.singleLine) correctSingleLineText else identity

    class Backend($: BackendScope[Props, Unit]) {

      val keyHandlers =
        KeyHandlers.commitAndAbort($.props.abort, $.props.commit(parseState), t.singleLine).tagMod

      val updateState: ReactEventI => IO[Unit] =
        e => $.props.stateUpdate(correctOnChange(e.target.value))

      def parseState = {
        val p = $.props
        t.parse(p.project.value())(p.state)
      }

      def render: ReactElement = {
        val p = $.props

        def editor =
          <.textarea(
            *.cellEditor(IsOK),
            keyHandlers,
            ^.ref       := textEditorRef,
            ^.value     := p.state,
            ^.onChange ~~> updateState)

        def preview =
          <.div(*.textEditPreview, p.projectWidgets.value() format parseState)

        <.div(editor, preview)
      }
    }
  }

  // ===================================================================================================================

  object GenericReqTitle   extends Base("GenericReqDesc editor",    Text.GenericReqTitle)
  object ReqCodeGroupTitle extends Base("ReqCodeGroupTitle editor", Text.ReqCodeGroupTitle)
  object CustomTextField   extends Base("CustomTextField editor",   Text.CustomTextField)
}
