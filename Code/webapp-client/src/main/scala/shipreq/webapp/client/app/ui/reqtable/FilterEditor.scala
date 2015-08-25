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
import shipreq.webapp.client.data.DataReusability._
import shipreq.webapp.client.lib.ui.UI
import shipreq.webapp.client.lib.{ShowDead, Contextualise}

object FilterEditor {

  type AutoComplete = ReusableVal[TC.Strategies]

  type OnFailure = State ~=> Callback
  type OnSuccess = (State, Option[FilterAst]) ~=> Callback

  case class Props(project  : Project,
                   onFailure: OnFailure,
                   onSuccess: OnSuccess,
                   state    : State)

  case class State(text: String, error: Option[String])

  implicit val reusabilityState = Reusability.by((_: State).text)
  implicit val reusabilityProps = Reusability.caseClass[Props]

  def initialState = State("", None)

  private val textEditorRef = Ref[HTMLTextAreaElement]("i")

  val Component =
    ReactComponentB[Props]("Filter")
      .backend(new Backend(_))
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

  class Backend($: BackendScope[Props, Unit]) {
    val project = Px.thunkM($.props.project)

    val autoComplete: Px[AutoComplete] =
      project.map { p =>
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
        $.propsCB >>= (_ onFailure State(text, Some(error)))

      def succeed(filter: Option[FilterAst]): Callback =
        $.propsCB >>= (_.onSuccess(State(text, None), filter))

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
            FilterAst(project.value(), spec) match {
              case \/-(ast) => succeed(Some(ast))
              case -\/(err) => fail(err)
            }
        }
      }
    }

    val filterBase =
      <.textarea(^.ref := textEditorRef, ^.onChange ==> onChange)

    def render: ReactElement = {
      Px.refresh(project)
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
