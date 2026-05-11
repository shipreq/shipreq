package shipreq.webapp.client.project.widgets

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import scalacss.ScalaCssReact._
import shipreq.base.util.{Invalid, Valid, Validity}
import shipreq.webapp.base.ui.semantic.{Button, Icon, Input}
import shipreq.webapp.client.project.app.Style.reqtable.{filterEditor => *}
import shipreq.webapp.member.feature.AutoCompleteFeature
import shipreq.webapp.member.feature.AutoCompleteFeature._
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.data.derivation.NaTags
import shipreq.webapp.member.project.filter._
import shipreq.webapp.member.project.issue.IssueCategory
import shipreq.webapp.member.project.util.DataReusability._
import shipreq.webapp.member.ui.AutosizeInput

/** Widget that allows users to edit the current filter.
  *
  * [ Filter...              ] [?]
  */
object FilterEditor {

  type UpdateFn = (State, Option[Filter.Valid], Callback) => Callback

  final case class Props(state  : State,
                         project: Project,
                         update : UpdateFn) {
    @inline def render = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.caseClassExcept("update") // used via $.props.flatMap in event handler which is reuse-safe

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
      .search(AutoComplete.Query caseInsensitiveStartsWith Seq("field", "has", "no", "implies", "impliedBy"))
      .replace("$1" + _ + ":")
      .result()

  private val autoCompletePresenceLackAttr: AutoComplete.Strategy =
    AutoComplete.Strategy.builder
      .regex("""\b((?:has|no):)([a-z]*)$""", index = 2)
      .search(AutoComplete.Query caseInsensitiveStartsWith FilterAst.Attr.values.iterator.map(_.name).toSeq)
      .replace("$1" + _ + " ")
      .result()

  private val autoCompleteHasIssue: AutoComplete.Strategy = {
    val values = IssueCategory.values.iterator.map(FilterAst.issueCategoryToStr).toSeq
    AutoComplete.Strategy.builder
      .regex("""\b(has:issue:-?(?:[a-z]+?,)*)([a-z]*)$""", index = 2)
      .search(AutoComplete.Query caseInsensitiveStartsWith values)
      .replace("$1" + _ + " ")
      .result()
  }

  private val correctInput: String => String = {
    val newlines = """\s*[\n\r]\s*""".r
    s => newlines.replaceAllIn(s, " ").trim
  }

  def parseAndValidate(input: String, validator: Filter.Validator): (Validity, Option[Filter.Valid]) =
    Filter.parseAndValidate(correctInput(input), validator) match {
      case \/-(f) => (Valid,   f)
      case -\/(_) => (Invalid, None)
    }

  private def normaliseAutoCompleteField(s: String): String =
    s.filter(FilterAlgebra.isFieldNameUnquotedChar).toLowerCase

  private def normaliseAutoCompleteFieldAttr(s: String): String =
    s.toLowerCase

  private final class FieldSuggestion(val display: String, value: String) {
    val displayNormalised = normaliseAutoCompleteField(display)
    def quotedValue = FilterAlgebra.quoteFieldName(value)
  }

  private object FieldSuggestion {
    @inline def apply(value: String): FieldSuggestion =
      apply(value, value)

    def apply(display: String, value: String): FieldSuggestion =
      new FieldSuggestion(display, value)
  }

  private final class FieldAttrSuggestion(val attr: FilterAst.FieldAttr) {
    def value = attr.name
    val normalised = normaliseAutoCompleteFieldAttr(value)
  }

  private object FieldAttrSuggestion {
    val all =
      FilterAst.FieldAttr.values
        .map(new FieldAttrSuggestion(_))
        .sortBy(_.normalised)
  }

  private val specialFields: ArraySeq[FieldSuggestion] =
    SpecialBuiltInField.filterOk
      .iterator
      .map(f => FieldSuggestion(f.name))
      .to(ArraySeq)

  private[widgets] def autoCompleteStrategies(p: Project): AutoComplete.Strategies = {

    val hashtags = AutoComplete.Project.hashtag(
      project    = p,
      filterDead = ShowDead,
      issues     = true,
      tags       = true,
      naTags     = NaTags.none)(
      Contextualise)

    val fieldNames: ArraySeq[FieldSuggestion] = {
      lazy val allNames: Set[String] =
        p.config.fieldsByName.iterator.map(_._1).toSet ++ SpecialBuiltInField.filterOk.iterator.map(_.name)

      def projectFields =
        p.config.fieldsByName
          .iterator
          .filter(_._2 match {
            case _: CustomField
               | StaticField.AllTags
               | StaticField.OtherTags
               | StaticField.NormalAltStepTree
               | StaticField.ExceptionStepTree
            => true
            case StaticField.ImplicationGraph
               | StaticField.StepGraph
            => false
          })
          .map { case (name, field) =>
            def value: String =
              field match {
                case f: CustomField.Implication =>
                  val reqType = p.config.reqTypes.need(f.reqTypeId).mnemonic.value
                  if (allNames.contains(reqType))
                    name
                  else
                    reqType
                case _ =>
                  name
              }
            FieldSuggestion(name, value)
          }

      MutableArray(projectFields ++ specialFields)
        .sortBy(_.display)
        .arraySeq
    }

    val fieldNameLookup: String => IterableOnce[FieldSuggestion] =
      input => {
        val i = normaliseAutoCompleteField(input)
        fieldNames.iterator
          .filter(_.displayNormalised contains i)
          .take(AutoCompleteFeature.MaxResults)
      }

    val derivationField =
      AutoComplete.Strategy.builder
        .regex("""\b\+?(derivation\() *([^ )]*)$""", index = 2)
        .search(fieldNameLookup)
        .replace(f => "$1" + f.quotedValue + "):(")
        .template((f, _) => f.display)
        .result()

    val fieldName =
      AutoComplete.Strategy.builder
        .regex("""\b(field:)([^ :=]*)$""", index = 2)
        .search(fieldNameLookup)
        .replace(f => "$1" + f.quotedValue)
        .template((f, _) => f.display)
        .result()

    val fieldAttr = {
      val query: String => IterableOnce[String] =
        input => {
          val n = normaliseAutoCompleteFieldAttr(input)
          FieldAttrSuggestion.all.iterator.filter(_.normalised.startsWith(n)).map(_.value)
        }

      AutoComplete.Strategy.builder
        .regex("""\b(field:(?:"[^"]+"|[^ :=]+)=)([^ ]*)$""", index = 2)
        .search(query)
        .replace("$1" + _)
        .result()
    }

    hashtags :+ derivationField :+ fieldName :+ fieldAttr :+ autoCompletePresenceLackAttr :+ autoCompleteHasIssue :+ autoCompleteKeywords
  }

  final class Backend($: BackendScope[Props, Unit]) extends AutoComplete.BackendI {

    override protected def getTextFromHeadToCaret =
      AutoComplete.getTextFromHeadToCaretI

    private val pxProject: Px[Project] =
      Px.props($).map(_.project).withReuse.autoRefresh

    private val pxProjectConfig: Px[ProjectConfig] =
      Px.props($).map(_.project.config).withReuse.autoRefresh

    private val pxFilterValidator: Px[Filter.Validator] =
      pxProjectConfig.map(FilterAlgebra.validate)

    private val pxAutoComplete: Px[AutoComplete.Strategies] =
      pxProject.map(autoCompleteStrategies)

    private val helpButton: VdomTag =
      Button(tipe = Button.Type.IconOnly(Icon.HelpCircle))
        .tag(^.onClick --> FilterHelp.modal.show)

    private val clearButton: VdomTag =
      Button(tipe = Button.Type.IconOnly(Icon.Close))
        .tag(^.onClick --> $.props.flatMap(_.update(State.init, None, Callback.empty)))

    def updateFilterText(input: String, cb: Callback): Callback =
      for {
        v <- pxFilterValidator.toCallback
        r = parseAndValidate(input, v)
        p <- $.props
        _ <- p.update(State(input, r._1), r._2, cb)
      } yield ()

    val inputNode = Ref[html.Input]

    override val autoCompleteCtx =
      inputNode.get.asCBO.map(AutoCompleteCtx(pxAutoComplete.value(), _))

    private lazy val inputTagMod = TagMod(
      ^.onBlur           --> autoCompleteOnBlur,
      ^.onClick          ==> autoCompleteOnClick,
      ^.onKeyDown        ==> autoCompleteOnKeyDown,
      ^.onKeyDownCapture ==> autoCompleteOnKeyDownCapture,
      ^.placeholder       := "Filter...",
      ^.minWidth          := "32ex",
    )

    private val inputExtraWidth =
      Some("2.67142857em + 4ex") // filter-icon-width + extra

    def render(p: Props): VdomElement = {

      var filterIcon =
        Icon.Filter.tag

      var inputTagMod =
        this.inputTagMod

      var onRight: TagMod =
        helpButton

      if (correctInput(p.state.text).nonEmpty) {
        filterIcon  = filterIcon(*.filterIcon(p.state.validity))
        inputTagMod = TagMod(inputTagMod, *.input(p.state.validity))
        onRight     = TagMod(clearButton, onRight)
      }

      val input =
        AutosizeInput.Props(
          state      = StateSnapshot(p.state.text)((os, cb) => os.fold(cb)(updateFilterText(_, cb))),
          tagMod     = inputTagMod,
          ref        = Some(inputNode),
          extraWidth = inputExtraWidth,
        )

      Input.Text.iconAndRightAction(
        filterIcon,
        input.render,
        onRight,
        p.state.validity)
    }
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .configure(AutoComplete.install(autoCompletableInput))
    .build
}
