package shipreq.webapp.client.app.ui

import japgolly.scalajs.jquery.{TextComplete => TC}
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import org.scalajs.dom.ext.KeyValue
import scala.scalajs.js
import scalaz.effect.IO
import scalaz.std.option._
import scalaz.std.stream._
import scalaz.std.vector._
import scalaz.syntax.foldable._
import scalaz.{-\/, Tags, \/}

import shipreq.base.util.Px
import shipreq.webapp.client.lib.ui.{KeyHandlers, TextEditor, UI}
import shipreq.webapp.client.util.IsOK
import TextSeqEditor._

object TextSeqEditor {
  type ParseRejection  = Option[String]
  type ParseResult[+O] = ParseRejection \/ O
  type Parser[+O]      = () => String => ParseResult[O]
  type AutoComplete    = Px[TC.Strategies]

  val leftNone: ParseResult[Nothing] =
    -\/(None)
}

/**
 * Text editor for a sequence of A's.
 *
 * Example: "#tbd #report #pending" or "5,7,9,11".
 */
final class TextSeqEditor[A](name         : String,
                             splitFn      : String => Stream[String],
                             textEditor   : TextEditor,
                             inputStyle   : IsOK => TagMod,
                             errorMsgStyle: TagMod) {

  case class Props(state       : String,
                   stateUpdate : String => IO[Unit],
                   abort       : IO[Unit],
                   parser      : Parser[A],
                   commit      : Vector[A] => IO[Unit],
                   autoComplete: AutoComplete) {
    def apply = component(this)
  }

  @inline private implicit def impTextEditor = textEditor.asImplicit

  private val textEditorRef = Ref[textEditor.Dom]("i")

  val component =
    ReactComponentB[Props](name)
      .stateless
      .backend(new Backend(_))
      .render(_.backend.render)
      .componentDidMount { $ =>
        val n = textEditorRef($).get.getDOMNode()
        textEditor.focus(n)
        textEditor.select(n)

        // TODO Should update autoComplete if needed on props change
        val strategies = $.props.autoComplete.value()
        UI.textComplete(n, strategies, $.props.stateUpdate)
      }
      .build

  class Backend($: BackendScope[Props, Unit]) {

    val updateState: ReactEventI => IO[Unit] =
      e => $.props.stateUpdate(e.target.value)

    def render: ReactElement = {
      val p = $.props

      val parse = p.parser()
      val parseResult =
        splitFn(p.state)
          .map(parse(_).bimap(Tags.First.apply, Vector.empty :+ _))
          .suml

      val keyHandlers =
        KeyHandlers.commitAndAbort(p.abort, parseResult.fold(_ => js.undefined, p.commit), textEditor.singleLine)

      <.div(
        textEditor.tag(
          inputStyle(IsOK(parseResult)),
          keyHandlers,
          ^.ref         := textEditorRef,
          ^.value       := p.state,
          ^.onChange   ~~> updateState),
        parseResult.swap.toOption.flatMap(Tags.First.unwrap).map(err =>
          <.div(errorMsgStyle, err)
        ))
    }
  }
}