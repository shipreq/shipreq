/*
package shipreq.webapp.client.project.app.reqtable

import japgolly.scalajs.react._, vdom.html_<^._
import japgolly.scalajs.react.extra._
import org.parboiled2.ErrorFormatter
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import scalaz.{-\/, \/-}
import shipreq.base.util.Valid
import shipreq.webapp.base.data.{Project, ShowDead}
import shipreq.webapp.base.filter._
import shipreq.webapp.client.base.data.Contextualise
import shipreq.webapp.client.base.jsfacade.{TextComplete => TC}
import shipreq.webapp.client.project.app.Style.reqtable.{filterEditor => *}
import shipreq.webapp.client.project.lib.AutoComplete
import shipreq.webapp.client.project.lib.DataReusability._
import shipreq.webapp.client.project.feature.AutoCompleteFeature

object FilterEditor {

  type OnFailure = State ~=> Callback
  type OnSuccess = (State, Option[ValidFilter]) ~=> Callback

  case class Props(project  : Project,
                   onFailure: OnFailure,
                   onSuccess: OnSuccess,
                   state    : State)

  case class State(text: String, error: Option[String])

  implicit val reusabilityState = Reusability.by((_: State).text)
  implicit val reusabilityProps = Reusability.caseClass[Props]

  def initialState = State("", None)

  val Component =
    ScalaComponent.builder[Props]("Filter")
      .renderBackend[Backend]
      .configure(
        shouldComponentUpdate,
        AutoCompleteFeature.installB(_.backend.textarea, _.autoComplete.value(), _.updateFilterText))
      .build

  private val acCommand: TC.Strategy =
    TC.Strategy.pattern("""(^|[^\w:])([a-z]+)$""", index = 2)
      .search(TC caseInsensitiveStartsWith Stream("has", "no", "implies", "impliedBy"))
      .replace("$1" + _ + ":")

  private val acPresenceLackAttr: TC.Strategy =
    TC.Strategy.pattern("""\b((?:has|no):)([a-z]*)$""", index = 2)
      .search(TC caseInsensitiveStartsWith ValidFilter.Attr.values.toStream.map(_.name))
      .replace("$1" + _ + " ")

  class Backend($: BackendScope[Props, Unit]) {
    var textarea: html.TextArea = _

    val project = Px.props($).map(_.project).withReuse.manualRefresh


    val autoComplete: Px[AutoCompleteFeature.Strategies] =
      project.map { p =>
        val hashtags = AutoComplete.hashtag(p, ShowDead, issues = true, tags = true)(Contextualise)
        hashtags :+ acPresenceLackAttr :+ acCommand
      }

    val parseErrorFormatter = new ErrorFormatter(
      showExpected         = false,
      showPosition         = true,
      showLine             = true,
      showTraces           = false,
      showFrameStartOffset = true)

    val inputCorrect = """\s*[\n\r]\s*""".r

    val onChange: ReactEventFromTextArea => Callback =
      e => updateFilterText(inputCorrect.replaceAllIn(e.target.value, " "))

    def updateFilterText(text: String): Callback = {

      def fail(error: String): Callback =
        $.props >>= (_ onFailure State(text, Some(error)))

      def succeed(filter: Option[ValidFilter]): Callback =
        $.props >>= (_.onSuccess(State(text, None), filter))

      FilterParser.parse(text) match {
        case e: FilterParser.Result.ParseException   => fail(e.format(parseErrorFormatter))
        case FilterParser.Result.GeneralException(e) => fail(e.getMessage)
        case FilterParser.Result.BlankFilter         => succeed(None)
        case FilterParser.Result.Filter(pf)          =>
          PotentialFilter.validator(project.value())(pf) match {
            case \/-(f) => succeed(Some(f))
            case -\/(e) => fail(e)
          }
      }
    }

    val filterBase =
      <.textarea.ref(textarea = _)(^.onChange ==> onChange)

    def render(p: Props): VdomElement = {
      Px.refresh(project)
      val s = p.state
      <.div(
        filterBase(
          *.editor(Valid when s.error.isEmpty),
          ^.value := s.text),
        s.error.whenDefined(err =>
          <.div(*.errorMsg, err)))
    }
  }
}
*/
