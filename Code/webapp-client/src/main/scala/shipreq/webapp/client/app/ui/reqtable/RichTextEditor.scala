package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._
import japgolly.scalajs.jquery.{TextComplete => TC}
import japgolly.scalacss.ScalaCssReact._
import org.scalajs.dom.ext.KeyValue
import org.scalajs.dom.raw.HTMLTextAreaElement
import shipreq.webapp.client.app.ui.ProjectWidgets
import scalajs.js
import scalaz.effect.IO
import shipreq.base.util.Rx
import shipreq.webapp.base.data._
import shipreq.webapp.base.text._
import shipreq.base.util.ScalaExt._
import shipreq.base.util.{Must, UnivEq, Rx}
import shipreq.base.util.effect.IoUtils, IoUtils.IoExt
import shipreq.webapp.base.UiText
import shipreq.webapp.base.text.{Grammar, Presentation}
import shipreq.webapp.client.app.ui.Style.{reqtable => *}
import shipreq.webapp.client.lib.ui.UI

// TODO Limit size


object RichTextEditor {

  type AutoComplete = Rx[TC.Strategies]
  type S = String

  abstract class Base[TextType <: Text.Generic](name: String, final val t: TextType) {

    case class Props(state         : S,
                     stateUpdate   : S => IO[Unit],
                     abort         : IO[Unit],
                     commit        : t.OptionalText => IO[Unit],
                     project       : Rx[Project],
                     projectWidgets: Rx[ProjectWidgets],
                     autoComplete  : AutoComplete)

    val textEditorRef = Ref[HTMLTextAreaElement]("i")

    val component =
      ReactComponentB[Props](name)
        .stateless
        .backend(new Backend(_))
        .render(_.backend.render)
        .componentDidMount { $ =>
          val n = textEditorRef($).get.getDOMNode()
          n.focus()
          n.select()

          // TODO Should update autoComplete if needed on props change
          val strategies = $.props.autoComplete.value()
          UI.textComplete(n, strategies, $.props.stateUpdate)
        }
        .build

    class Backend($: BackendScope[Props, Unit]) {

      val cancelOnEscape = UI.keyDispatch(_.key) {
        case KeyValue.Escape => $.props.abort
      }

      val onChange: ReactEventI => IO[Unit] =
        e => $.props.stateUpdate(e.target.value)

      //val textToString = $.props.project.map(Presentation.textToString)

      def render: ReactElement = {
        val p = $.props

        // TODO prevent NLs in SingleLine

//        def onKeyPress = UI.keyDispatch(_.key) {
//          case KeyValue.Enter => parseResult.fold(_ => js.undefined, p.commit)
//        }

        val parsed = t.parse(p.project.value())(p.state)

        def editor =
          <.textarea(
            ^.ref := textEditorRef,
            *.cellEditor(false),
            ^.value       := p.state,
            ^.onChange   ~~> onChange,
            //^.onKeyPress ~~> onKeyPress,
            ^.onKeyDown  ~~> cancelOnEscape)

        def preview =
          <.div(*.textEditPreview, p.projectWidgets.value().text(parsed))

        <.div(editor, preview)
      }
    }

    def cellState(p: Props): Cell.Editing =
      Cell.Editing(component(p))
  }

  object GenericReqDesc extends Base("GenericReqDescEditor", Text.GenericReqDesc) {

    def apply(initial : t.OptionalText,
              project : Rx[Project],
              projectWidgets: Rx[ProjectWidgets],
              setState: Option[Cell.State] => IO[Unit]): Cell.State = {

      def init: S =
        Presentation.textToString(project.value())(initial)

      val abort: IO[Unit] =
        setState(None)

      val commit: t.OptionalText => IO[Unit] =
      // TODO If change occurred, send to server & lock cell. (If unchanged, clear state.)
        s => setState(None) >>> IO{ println("Sent to ze server: " + s) }

      def autoComplete: AutoComplete =
        Rx(TC.Strategies()).noReuse

      lazy val update: S => IO[Unit] =
        s => setState(Some(newState(s)))

      def newState(state: S) =
        cellState(Props(state, update, abort, commit, project, projectWidgets, autoComplete))

      newState(init)


    }

  }
}