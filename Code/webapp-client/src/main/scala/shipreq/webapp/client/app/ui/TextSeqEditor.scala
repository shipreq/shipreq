package shipreq.webapp.client.app.ui

import japgolly.scalajs.jquery.{TextComplete => TC}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.extra._
import scalaz.{\/, -\/, Tags}
import scalaz.std.option._
import scalaz.std.stream._
import scalaz.std.vector._
import scalaz.syntax.foldable._
import shipreq.webapp.client.lib.ui.{KeyHandlers, TextEditor, UI}
import shipreq.base.util.Validity
import TextSeqEditor._

object TextSeqEditor {
  type ParseRejection  = Option[String]
  type ParseResult[+O] = ParseRejection \/ O
  type Parser[+O]      = String => ParseResult[O]
  type AutoComplete    = ReusableVal[TC.Strategies]

  val leftNone: ParseResult[Nothing] =
    -\/(None)
}

/**
 * Text editor for a sequence of A's.
 *
 * Example: "#tbd #report #pending" or "5,7,9,11".
 */
final class TextSeqEditor[A, B](name: String, splitFn: String => Stream[String], textEditor: TextEditor) {

  case class Props(vuca         : VUCA[String, B],
                   parser       : Parser[A],
                   validate     : Vector[A] => ParseResult[B],
                   autoComplete : AutoComplete,
                   inputStyle   : Validity => TagMod,
                   errorMsgStyle: TagMod) {

    val parseResult: ParseResult[B] =
      splitFn(vuca.value)
        .map(parser(_).bimap(Tags.First.apply, Vector.empty :+ _))
        .suml
        .leftMap(Tags.First.unwrap)
        .flatMap(validate)

    def render = component(this)
  }

  @inline private implicit def impTextEditor = textEditor.asImplicit

  private val textEditorRef = Ref[textEditor.Dom]("i")

  val component =
    ReactComponentB[Props](name)
      .renderBackend[Backend]
      .configure(UI.installTextCompleteP(textEditorRef, _.autoComplete, _.vuca.update))
      .build

  class Backend($: BackendScope[Props, Unit]) {

    val updateState: ReactEventI => Callback =
      e => $.props >>= (_.vuca.update(e.target.value))

    def render(p: Props): ReactElement = {
      val parseResult = p.parseResult

      val keyHandlers =
        KeyHandlers.commitAndAbortD(p.vuca.abort, parseResult, p.vuca.commit, textEditor.singleLine)

      <.div(
        textEditor.tag(
          p.inputStyle(Validity(parseResult)),
          keyHandlers,
          ^.ref       := textEditorRef,
          ^.value     := p.vuca.value,
          ^.onChange ==> updateState),
        parseResult.fold(
          _.fold(EmptyTag)(err => <.div(p.errorMsgStyle, err)),
          _ => EmptyTag))
    }
  }
}