package shipreq.webapp.client.app.ui.reqtable

import japgolly.scalajs.jquery.{TextComplete => TC}
import japgolly.scalajs.react._, vdom.prefix_<^._
import japgolly.scalajs.react.extra._
import org.parboiled2.{ErrorFormatter, ParseError}
import org.scalajs.dom.html
import org.scalajs.dom.raw.HTMLTextAreaElement
import scala.util.{Failure, Success}
import scalacss.ScalaCssReact._
import scalaz.{-\/, \/-}
import shipreq.base.util.Valid
import shipreq.webapp.base.data.Project
import shipreq.webapp.base.filter._
import shipreq.webapp.client.app.ui.Style.reqtable.{filterEditor => *}
import shipreq.webapp.client.app.ui.reqtable.edit.AutoComplete
import shipreq.webapp.client.lib.ui.UI
import shipreq.webapp.client.lib.{ShowDead, Contextualise}

object FilterEditor {

  type AutoComplete = ReusableVal[TC.Strategies]

  case class StaticProps(project  : Px[Project],
                         onFailure: State => Callback,
                         onSuccess: (State, Option[FilterAst]) => Callback)

  case class State(text: String, error: Option[String])

  type Props = State

  implicit val stateReuse = Reusability.by((_: State).text)

  def initialState = State("", None)

  private val textEditorRef = Ref[HTMLTextAreaElement]("i")

  def component(staticProps: StaticProps) =
    ReactComponentB[Props]("Filter")
      .stateless
      .backend(new Backend(staticProps, _))
      .render(_.backend.render)
      .configure(
        UI.installTextCompleteB(textEditorRef, _.autoComplete.value(), _.updateFilterText),
        shouldComponentUpdate)
      .build


  private val acCommand =
    TC.Strategy.pattern("""(^|[^\w:])([a-z]+)$""", index = 2)
      .search(TC caseInsensitiveStartsWith Stream("has", "no", "implies", "impliedBy"))
      .replace("$1" + _ + ":")

  private val acPresenceLackAttr =
    TC.Strategy.pattern("""\b((?:has|no):)([a-z]*)$""", index = 2)
      .search(TC caseInsensitiveStartsWith FilterAst.Attr.values.toStream.map(_.name))
      .replace("$1" + _ + " ")

  class Backend(sp: StaticProps, $: BackendScope[Props, Unit]) {

    val autoComplete: Px[AutoComplete] =
      sp.project.map { p =>
        val hashtag = AutoComplete.hashtag(p, ShowDead, issues = true, tags = true)(Contextualise)
        val ac = TC.Strategies(hashtag, acPresenceLackAttr, acCommand)
        ReusableVal.byRef(ac)
      }

    val parseErrorFormatter = new ErrorFormatter(
      showExpected         = false,
      showPosition         = true,
      showLine             = true,
      showTraces           = false,
      showFrameStartOffset = true)

    val inputCorrect = """\s*[\n\r]\s*""".r

    val onChange: ReactEventTA => Callback =
      e => updateFilterText(inputCorrect.replaceAllIn(e.target.value, " "))

    def updateFilterText(text: String): Callback = {
      def fail(error: String): Callback =
        sp.onFailure(State(text, Some(error)))

      def succeed(filter: Option[FilterAst]): Callback =
        sp.onSuccess(State(text, None), filter)

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
            FilterAst(sp.project.value(), spec) match {
              case \/-(ast) => succeed(Some(ast))
              case -\/(err) => fail(err)
            }
        }
      }
    }

    val filterBase =
      <.textarea(^.ref := textEditorRef, ^.onChange ==> onChange)

    def render: ReactElement = {
      val s = $.props
      <.div(
        filterBase(
          *.editor(Valid <~ s.error.isEmpty),
          ^.value := s.text),
        s.error.map(err =>
          <.div(*.errorMsg, err)))
    }
  }
}
