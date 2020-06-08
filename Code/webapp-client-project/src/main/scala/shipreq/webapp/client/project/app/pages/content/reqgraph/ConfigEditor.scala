package shipreq.webapp.client.project.app.pages.content.reqgraph

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scala.collection.immutable.ArraySeq
import shipreq.base.util.NonEmptyArraySeq
import shipreq.webapp.base.UiText
import shipreq.webapp.base.data.{Dead, FilterDead, ProjectConfig, SpecialBuiltInField, TagGroupId, Tags}
import shipreq.webapp.base.data.savedview.ImpGraphConfig
import shipreq.webapp.base.data.savedview.ImpGraphConfig.{Colours, GraphDir, LabelFormat}
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.ui.widgets.Dropdown
import scalacss.ScalaCssReact._
import shipreq.webapp.base.ui.semantic.Icon
import shipreq.webapp.client.project.app.Style.{reqgraphPage => *}

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

    private val coloursKey: Colours => Dropdown.ItemKey = {
      case Colours.ByReqType => "r"
      case Colours.ByTag(id) => "t" + id.value.toString
    }

    private type Unsorted = (String, Dropdown.Item[Colours])

    private val staticItems: ArraySeq[Unsorted] =
      ArraySeq(
        UiText.FieldNames.fieldType -> Dropdown.Item(
          key = coloursKey(Colours.ByReqType),
          label = UiText.FieldNames.fieldType,
          value = Colours.ByReqType,
        ),
      )

    private val pxColourOptions: Px[NonEmptyArraySeq[Dropdown.Item[Colours]]] =
      for {
        tags     <- pxTags
        fd       <- pxFilterDead
        selected <- pxSelectedColours
      } yield {

        var extraTagGroupId: Option[TagGroupId] =
          selected match {
            case Colours.ByReqType => None
            case Colours.ByTag(id) => Some(id)
          }

        var tagGroups =
          fd.filterFn.iteratorBy(tags.tagGroupIterator())(_.live)

            .tapEach(tg => if (extraTagGroupId.contains(tg.id)) extraTagGroupId = None)
            .toVector

        for (id <- extraTagGroupId)
          tagGroups :+= tags.needTagGroup(id)

        def tagGroupItems: Iterator[Unsorted] =
          tagGroups.iterator.map { g =>
            val txt = "Tag: " + g.name
            val col = Colours.ByTag(g.id)
            txt -> Dropdown.Item(
              key = coloursKey(col),
              label = <.span(*.deadDropdownItem.when(g.live is Dead), txt),
              value = col,
            )
          }

        def all: Iterator[Unsorted] =
          staticItems.iterator ++ tagGroupItems

        NonEmptyArraySeq.force(MutableArray(all).sortBy(_._1).map(_._2).arraySeq)
      }

    def render(p: Props): VdomNode = {

      val graphDir =
        Dropdown.Props.NonEmpty(
          items    = graphDirOptions,
          selected = graphDirKey(p.state.value.graphDir))(
          onChange = i => p.state.modState(_.copy(graphDir = i.value))
        ).render

      val labelFormat =
        Dropdown.Props.NonEmpty(
          items    = labelFormatOptions,
          selected = labelFormatKey(p.state.value.labelFormat))(
          onChange = i => p.state.modState(_.copy(labelFormat = i.value))
        ).render

      val colours =
        Dropdown.Props.NonEmpty(
          items    = pxColourOptions.value(),
          selected = coloursKey(p.state.value.colours))(
          onChange = i => p.state.modState(_.copy(colours = i.value))
        ).render

      <.div(
        graphDir,
        labelFormat,
        colours,
      )
    }
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}
