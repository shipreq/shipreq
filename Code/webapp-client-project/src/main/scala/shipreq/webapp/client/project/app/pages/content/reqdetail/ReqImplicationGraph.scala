package shipreq.webapp.client.project.app.pages.content.reqdetail

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util.NonEmptyArraySeq
import shipreq.webapp.base.feature.TableNavigationFeature
import shipreq.webapp.base.ui.widgets.Dropdown
import shipreq.webapp.client.project.app.Style.reqdetail.{impGraph => *}
import shipreq.webapp.client.project.util.GraphColours
import shipreq.webapp.client.project.widgets.ImplicationGraph
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.data.savedview.ImpGraphConfig.Colours
import shipreq.webapp.member.project.util.DataReusability._

object ReqImplicationGraph {

  final case class Props(graph: ImplicationGraph.Props.FocusReq,
                         state: StateSnapshot[State]) {
    @inline def render: VdomElement = Component(this)
  }

  final case class State(colours: Option[Colours])

  object State {
    def init: State =
      State(None)

    implicit val reusability: Reusability[State] =
      Reusability.derive
  }

  private def render(p: Props): VdomNode =
    <.div(*.container,
      Controls.Props(p.graph.project.config, p.state).render,
      p.graph.render)

  implicit val reusabilityProps: Reusability[Props] = Reusability.derive

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build

  // ===================================================================================================================

  object Controls {
    final case class Props(projectConfig: ProjectConfig,
                           state        : StateSnapshot[State]) {
      @inline def render: VdomElement = Component(this)
    }

    private val pxFilterDead: Px[FilterDead] =
      Px.constByValue(HideDead)

    private final val noneKey = "-"

    private val noColoursItem: Dropdown.Item[Option[Colours]] =
      Dropdown.Item(noneKey, "Implication", None)

    final class Backend($: BackendScope[Props, Unit]) {

      private val pxTags: Px[Tags] =
        Px.props($).map(_.projectConfig.tags).withReuse.autoRefresh

      private val pxSelectedColours: Px[Option[Colours]] =
        Px.props($).map(_.state.value.colours).withReuse.autoRefresh

      private val pxColourOptions: Px[NonEmptyArraySeq[Dropdown.Item[Option[Colours]]]] =
        GraphColours.pxOptions(pxTags, pxFilterDead, pxSelectedColours).map { items =>
          noColoursItem +: items.map(_.map(Some(_)))
        }

      def render(p: Props): VdomNode = {
        val s           = p.state.value
        val selectedKey = s.colours.fold(noneKey)(GraphColours.key)

        val dropdown =
          Dropdown.Props.NonEmpty(
            items    = pxColourOptions.value(),
            selected = selectedKey)(
            onChange = i => p.state.modState(_.copy(colours = i.value))
          ).render

        <.div(*.controls,
          TableNavigationFeature.ignoreFamily,
          <.div(*.controlHeader, "Colours"),
          dropdown)
      }
    }

    implicit val reusabilityProps: Reusability[Props] = Reusability.derive

    val Component = ScalaComponent.builder[Props]
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .build

  }
}
