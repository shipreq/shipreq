package shipreq.webapp.client.project.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra._
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import scalaz.{-\/, \/-}
import shipreq.base.util.{Invalid, Valid, Validity}
import shipreq.webapp.base.data.{Contextualise, Project, ProjectConfig, ShowDead}
import shipreq.webapp.base.filter._
import shipreq.webapp.base.feature.AutoCompleteFeature._
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.ui.semantic.{Button, Icon, Input}
import shipreq.webapp.client.project.app.Style.reqtable.{filterEditor => *}

/** Widget that allows users to edit the current filter.
  *
  * [ Filter...              ] [?]
  */
object FilterEditor {

  type UpdateFn = (State, Option[Filter.Valid]) => Callback

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
      Reusability.derive
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
      .search(AutoComplete.Utils caseInsensitiveStartsWith FilterAst.Attr.values.toStream.map(_.name))
      .replace("$1" + _ + " ")
      .result()

  private val correctInput: String => String = {
    val newlines = """\s*[\n\r]\s*""".r
    s => newlines.replaceAllIn(s, " ").trim
  }

  def parseAndValidate(input: String, validator: Filter.Validator): (Validity, Option[Filter.Valid]) =
    Filter.parseAndValidate(correctInput(input), validator) match {
      case \/-(f) => (Valid,   f)
      case -\/(_) => (Invalid, None)
    }

  final class Backend($: BackendScope[Props, Unit]) extends AutoComplete.BackendI {

    private val pxProject: Px[Project] =
      Px.props($).map(_.project).withReuse.autoRefresh

    private val pxProjectConfig: Px[ProjectConfig] =
      Px.props($).map(_.project.config).withReuse.autoRefresh

    private val pxFilterValidator: Px[Filter.Validator] =
      pxProjectConfig.map(FilterAlgebra.validate)

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
        r = parseAndValidate(input, v)
        p ← $.props
        _ ← p.update(State(input, r._1), r._2)
      } yield ()

    val inputNode = Ref[html.Input]

    override val autoCompleteCtx =
      inputNode.get.map(AutoCompleteCtx(pxAutoComplete.value(), _))

    def render(p: Props): VdomElement = {

      var filterIcon =
        Icon.Filter.tag

      var input =
        <.input.text(
          ^.onBlur     --> autoCompleteBlur,
          ^.onChange   ==> onChange,
          ^.placeholder := "Filter...",
          ^.value       := p.state.text)

      var onRight: TagMod =
        helpButton

      if (correctInput(p.state.text).nonEmpty) {
        filterIcon = filterIcon(*.filterIcon(p.state.validity))
        input      = input(*.input(p.state.validity))
        onRight    = TagMod(clearButton, onRight)
      }

      Input.Text.iconAndRightAction(
        filterIcon,
        input.withRef(inputNode),
        onRight,
        p.state.validity)
    }
  }

  val Component = ScalaComponent.builder[Props]("FilterEditor")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .configure(AutoComplete.install(autoCompletableInput))
    .build
}
