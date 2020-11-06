package shipreq.webapp.client.project.app.pages.content.reqgraph

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util.NonEmptyArraySeq
import shipreq.webapp.base.ui.semantic.Icon
import shipreq.webapp.base.ui.widgets.Dropdown
import shipreq.webapp.client.project.app.Style.{reqgraphPage => *}
import shipreq.webapp.client.project.util.GraphColours
import shipreq.webapp.member.project.data.savedview.ImpGraphConfig
import shipreq.webapp.member.project.data.savedview.ImpGraphConfig.{Colours, GraphDir, LabelFormat}
import shipreq.webapp.member.project.data.{FilterDead, ProjectConfig, SpecialBuiltInField, Tags}
import shipreq.webapp.member.project.util.DataReusability._

private[reqgraph] object ConfigEditor {

  final case class Props(state        : StateSnapshot[ImpGraphConfig],
                         filterDead   : FilterDead,
                         projectConfig: ProjectConfig) {
    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  private val graphDirKey: GraphDir => Dropdown.ItemKey = {
    case GraphDir.BottomToTop => "B"
    case GraphDir.LeftToRight => "L"
    case GraphDir.RightToLeft => "R"
    case GraphDir.TopToBottom => "T"
  }

  private val graphDirOptions = {
    val text: GraphDir => String = {
      case GraphDir.BottomToTop => "Bottom to top"
      case GraphDir.LeftToRight => "Left to right"
      case GraphDir.RightToLeft => "Right to left"
      case GraphDir.TopToBottom => "Top to bottom"
    }

    val icon: GraphDir => Icon = {
      case GraphDir.BottomToTop => Icon.ArrowUp
      case GraphDir.LeftToRight => Icon.ArrowRight
      case GraphDir.RightToLeft => Icon.ArrowLeft
      case GraphDir.TopToBottom => Icon.ArrowDown
    }

    NonEmptyArraySeq.fromNEV(
      GraphDir.values.map(d =>
        Dropdown.Item(
          key = graphDirKey(d),
          label = <.span(icon(d).tagNoMargin, " ", text(d)),
          value = d,
        )
      )
    )
  }

  private val labelFormatKey: LabelFormat => Dropdown.ItemKey = {
    case LabelFormat.Pubid         => "P"
    case LabelFormat.PubidAndTitle => "T"
  }

  private val labelFormatOptions = {
    val text: LabelFormat => String = {
      case LabelFormat.Pubid         => SpecialBuiltInField.Pubid.name
      case LabelFormat.PubidAndTitle => s"${SpecialBuiltInField.Pubid.name} and ${SpecialBuiltInField.Title.name}"
    }

    NonEmptyArraySeq.fromNEV(
      LabelFormat.values.map(d =>
        Dropdown.Item(
          key = labelFormatKey(d),
          label = text(d),
          value = d,
        )
      )
    )
  }

  final class Backend($: BackendScope[Props, Unit]) {

    private val pxTags: Px[Tags] =
      Px.props($).map(_.projectConfig.tags).withReuse.autoRefresh

    private val pxFilterDead: Px[FilterDead] =
      Px.props($).map(_.filterDead).withReuse.autoRefresh

    private val pxSelectedColours: Px[Colours] =
      Px.props($).map(_.state.value.colours).withReuse.autoRefresh

    private val pxColourOptions: Px[NonEmptyArraySeq[Dropdown.Item[Colours]]] =
      GraphColours.pxOptions(pxTags, pxFilterDead, pxSelectedColours.map(Some(_)))

    private val graphDirHeader = <.div(*.configGraphDirHeader, "Direction")
    private val labelsHeader   = <.div(*.configLabelsHeader, "Labels")
    private val coloursHeader  = <.div(*.configColoursHeader, "Colours")

    def render(p: Props): VdomNode = {

      val graphDirEditor =
        Dropdown.Props.NonEmpty(
          items    = graphDirOptions,
          selected = graphDirKey(p.state.value.graphDir))(
          onChange = i => p.state.modState(_.copy(graphDir = i.value))
        ).render

      val labelsEditor =
        Dropdown.Props.NonEmpty(
          items    = labelFormatOptions,
          selected = labelFormatKey(p.state.value.labelFormat))(
          onChange = i => p.state.modState(_.copy(labelFormat = i.value))
        ).render

      val coloursEditor =
        Dropdown.Props.NonEmpty(
          items    = pxColourOptions.value(),
          selected = GraphColours.key(p.state.value.colours))(
          onChange = i => p.state.modState(_.copy(colours = i.value))
        ).render

      <.div(*.configContainer,
        graphDirHeader, <.div(*.configGraphDirEditor, graphDirEditor),
        labelsHeader  , <.div(*.configLabelsEditor  , labelsEditor  ),
        coloursHeader , <.div(*.configColoursEditor , coloursEditor ))
    }
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}
