package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.react._, vdom.prefix_<^._, ScalazReact._, MonocleReact._
import japgolly.scalajs.react.extra._
import org.parboiled2.{ErrorFormatter, ParseError}
import org.scalajs.dom.html
import shipreq.webapp.client.util.Valid
import scala.util.{Failure, Success}
import scalacss.ScalaCssReact._
import scalaz.{-\/, \/-}
import scalaz.effect.IO
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.filter._
import shipreq.webapp.client.app.ui.Style.reqtable.{filterEditor => *}

object FilterEditor {

  // TODO Move to scalajs-react
  type ReactEventTA = SyntheticEvent[html.TextArea]

  case class State(text: String, error: Option[String])
  case class Props(state    : State,
                   project  : () => Project,
                   onFailure: State ~=> IO[Unit],
                   onSuccess: (State, Option[FilterAst]) ~=> IO[Unit])

  implicit val stateReuse = Reusability.by((_: State).text)
  implicit val propsReuse = Reusability.by((_: Props).state)
  // project, onSuccess, onFailure are read on demand in a static callback - reusability not needed

  def initialState = State("", None)

  val Component =
    ReactComponentB[Props]("Filter")
      .stateless
      .backend(new Backend(_))
      .render(_.backend.render)
      .configure(shouldComponentUpdate)
      .build

  class Backend($: BackendScope[Props, Unit]) {

    val parseErrorFormatter = new ErrorFormatter(
      showExpected         = false,
      showPosition         = true,
      showLine             = true,
      showTraces           = false,
      showFrameStartOffset = true)

    // Fix filter on blur?

    val onChange: ReactEventTA => IO[Unit] = e => {
      val text = e.target.value.replace("\n", " ")

      def fail(error: String): IO[Unit] =
        $.props.onFailure(State(text, Some(error)))

      def succeed(filter: Option[FilterAst]): IO[Unit] =
        $.props.onSuccess(State(text, None), filter)

      if (text.trim.isEmpty)
        succeed(None)
      else {
        // Parse
        val parser = new FilterParser(text)
        parser.main.run() match {
          case Failure(e: ParseError) => fail(parser.formatError(e, parseErrorFormatter))
          case Failure(e: Throwable)  => fail(e.getMessage)
          case Success(None)          => succeed(None)
          case Success(Some(spec))    =>
            FilterAst($.props.project(), spec) match {
              case \/-(ast) => succeed(Some(ast))
              case -\/(err) => fail(err)
            }
        }
      }
    }

    val filterBase =
      <.textarea(^.ref := "filter", ^.onChange ~~> onChange)

    def render: ReactElement = {
      val s = $.props.state
      <.div(
        filterBase(
          *.editor(Valid <~ s.error.isEmpty),
          ^.value := s.text),
        s.error.map(err =>
          <.div(*.errorMsg, err)))
    }
  }
}
