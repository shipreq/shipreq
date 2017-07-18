package shipreq.webapp.client.project.app.reqtable

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra._
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import scalaz.{-\/, \/-}
import shipreq.base.util.{Invalid, Valid, Validity}
import shipreq.webapp.base.data.{Project, ShowDead}
import shipreq.webapp.base.filter._
import shipreq.webapp.base.data.Contextualise
import shipreq.webapp.base.feature.AutoCompleteFeature._
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.ui.semantic.{Button, Icon, Input}
import shipreq.webapp.client.project.app.Style.reqtable.{filterEditor => *}
import shipreq.webapp.client.project.widgets.FilterHelp

/** Widget that allows users to edit the current filter.
  *
  * [ Filter...              ] [?]
  */
object FilterEditor {

  type UpdateFn = (State, Option[ValidFilter]) => Callback

  final case class Props(state  : State,
                         project: Project,
                         update : UpdateFn) {
    @inline def render = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.caseClassExcept('update) // used via $.props.flatMap in event handler which is reuse-safe

  final case class State(text: String, validity: Validity)

  object State {
    def init: State =
      State("", Valid)

    implicit val reusability: Reusability[State] =
      Reusability.caseClass
  }

  private val autoCompleteKeywords: AutoComplete.Strategy =
    AutoComplete.Strategy.builder
      .regex("""(^|[^\w:])([a-z]+)$""", index = 2)
      .search(AutoComplete.Utils caseInsensitiveStartsWith Stream("has", "no", "implies", "impliedBy"))
      .replace("$1" + _ + ":")
      .result()

  private val autoCompletePresenceLackAttr: AutoComplete.Strategy =
    AutoComplete.Strategy.builder
      .regex("""\b((?:has|no):)([a-z]*)$""", index = 2)
      .search(AutoComplete.Utils caseInsensitiveStartsWith ValidFilter.Attr.values.toStream.map(_.name))
      .replace("$1" + _ + " ")
      .result()

  private val correctInput: String => String = {
    val newlines = """\s*[\n\r]\s*""".r
    s => newlines.replaceAllIn(s, " ").trim
  }

  def parse(input: String, validator: PotentialFilter.Validator): (Validity, Option[ValidFilter]) =
    FilterParser.parse(correctInput(input)) match {
      case FilterParser.Result.Filter(pf)           => parsePF(pf, validator)
      case FilterParser.Result.GeneralException(_)
         | FilterParser.Result.ParseException(_, _) => (Invalid, None)
      case FilterParser.Result.BlankFilter          => (Valid, None)
    }

  def parsePF(pf: PotentialFilter, validator: PotentialFilter.Validator): (Validity, Option[ValidFilter]) =
    validator.run(pf) match {
      case \/-(f) => (Valid, Some(f))
      case -\/(_) => (Invalid, None)
    }

  /** Generated meaning the filter was created by code internally rather than being supplied externally */
  def parseGenerated(pf: PotentialFilter, validator: PotentialFilter.Validator): (State, Option[ValidFilter]) = {
    val txt = PotentialFilter.toText(pf)
    val p = parsePF(pf, validator)
    (State(txt, p._1), p._2)
  }

  final class Backend($: BackendScope[Props, Unit]) extends AutoComplete.BackendI {

    private val pxProject: Px[Project] =
      Px.props($).map(_.project).withReuse.autoRefresh

    private val pxFilterValidator: Px[PotentialFilter.Validator] =
      pxProject.map(PotentialFilter.validator)

    val pxAutoComplete: Px[AutoComplete.Strategies] =
      pxProject.map { p =>
        val hashtags = AutoComplete.Project.hashtag(p, ShowDead, issues = true, tags = true)(Contextualise)
        hashtags :+ autoCompletePresenceLackAttr :+ autoCompleteKeywords
      }

    private val helpButton: VdomTag =
      Button(tipe = Button.Type.IconOnly(Icon.HelpCircle))
        .tag(^.onClick --> FilterHelp.modal.show)

    private val clearButton: VdomTag =
      Button(tipe = Button.Type.IconOnly(Icon.Close))
        .tag(^.onClick --> $.props.flatMap(_.update(State.init, None)))

    private val onChange: ReactEventFromTextArea => Callback =
      e => updateFilterText(e.target.value)

    def updateFilterText(input: String): Callback =
      for {
        v ← pxFilterValidator.toCallback
        r = parse(input, v)
        p ← $.props
        _ ← p.update(State(input, r._1), r._2)
      } yield ()

    var inputNode: html.Input = _

    override val autoCompleteCtx =
      CallbackTo(
        Option(inputNode).map(
          AutoCompleteCtx(pxAutoComplete.value(), _)))

    def render(p: Props): VdomElement = {

      var filterIcon =
        Icon.Filter.tag

      var input =
        <.input.text(
          ^.placeholder := "Filter...",
          ^.value := p.state.text,
          ^.onChange ==> onChange)

      var onRight: TagMod =
        helpButton

      if (correctInput(p.state.text).nonEmpty) {
        filterIcon = filterIcon(*.filterIcon(p.state.validity))
        input      = input(*.input(p.state.validity))
        onRight    = TagMod(clearButton, onRight)
      }

      Input.Text.iconAndRightAction(
        filterIcon,
        input.ref(inputNode = _),
        onRight,
        p.state.validity)
    }
  }

  val Component = ScalaComponent.builder[Props]("FilterEditor")
    .renderBackend[Backend]
    .configure(shouldComponentUpdate)
    .configure(AutoComplete.install(autoCompletableInput))
    .build
}
