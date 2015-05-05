package shipreq.webapp.client.app.ui

import japgolly.scalajs.jquery.{TextComplete => TC}
import japgolly.scalajs.react.ScalazReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import scala.scalajs.js
import scalaz.effect.IO
import scalaz.std.option._
import scalaz.std.stream._
import scalaz.std.vector._
import scalaz.syntax.foldable._
import scalaz.{-\/, Tags, \/}

import shipreq.webapp.client.lib.ui.{KeyHandlers, TextEditor, UI}
import shipreq.webapp.client.util.{ReusableVal, IsOK}
import TextSeqEditor._

object TextSeqEditor {
  type ParseRejection  = Option[String]
  type ParseResult[+O] = ParseRejection \/ O
  type Parser[+O]      = () => String => ParseResult[O]
  type AutoComplete    = ReusableVal[TC.Strategies]

  val leftNone: ParseResult[Nothing] =
    -\/(None)
}

/**
 * Text editor for a sequence of A's.
 *
 * Example: "#tbd #report #pending" or "5,7,9,11".
 */
final class TextSeqEditor[A, B](name         : String,
                                splitFn      : String => Stream[String],
                                textEditor   : TextEditor,
                                inputStyle   : IsOK => TagMod,
                                errorMsgStyle: TagMod) {

  case class Props(state       : String,
                   stateUpdate : String => IO[Unit],
                   abort       : IO[Unit],
                   parser      : Parser[A],
                   validate    : Vector[A] => ParseResult[B],
                   commit      : B => IO[Unit],
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
      .configure(UI.installTextComplete2(textEditorRef, _.autoComplete, _.stateUpdate))
      .build

  class Backend($: BackendScope[Props, Unit]) {

    val updateState: ReactEventI => IO[Unit] =
      e => $.props.stateUpdate(e.target.value)

    def render: ReactElement = {
      val p = $.props
      val parse = p.parser()

      val parseResultA: ParseResult[Vector[A]] =
        splitFn(p.state)
          .map(parse(_).bimap(Tags.First.apply, Vector.empty :+ _))
          .suml
          .leftMap(Tags.First.unwrap)
      
      val parseResult: ParseResult[B] =
        parseResultA.flatMap(p.validate)

      val keyHandlers = KeyHandlers.commitAndAbort(
        p.abort,
        parseResult.fold(_ => js.undefined, p.commit),
        textEditor.singleLine)

      <.div(
        textEditor.tag(
          inputStyle(IsOK(parseResult)),
          keyHandlers,
          ^.ref       := textEditorRef,
          ^.value     := p.state,
          ^.onChange ~~> updateState),
        parseResult.fold(
          _.fold(EmptyTag)(err => <.div(errorMsgStyle, err)),
          _ => EmptyTag))
    }
  }
}