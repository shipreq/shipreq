package shipreq.webapp.client.base.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.ExternalVar
import japgolly.scalajs.react.vdom.prefix_<^._
import monocle.Lens
import monocle.macros.Lenses
import scalacss.ScalaCssReact._
import shipreq.webapp.base.URLs
import shipreq.webapp.base.UiText.EnglishStringExt
import shipreq.webapp.base.data.{ProjectCatalogue, Validators}
import shipreq.webapp.client.base.feature.{AsyncActionFeature, EditorStatus}
import shipreq.webapp.client.base.jsfacade.MomentJs
import shipreq.webapp.client.base.ui.semantic.{Icon, Size, Statistic, StatisticGroup}
import BaseStyles.{projectItems => *}

/** Project name and summary.
  *
  * +-----------------------------------------------+
  * | Pacman                              2090  503 |
  * | Updated 30 years ago.            CHANGES REQS |
  * +-----------------------------------------------+
  */
object ProjectItem {

  private val statGroupStyle = StatisticGroup.Style(Size.Tiny)

  private def stat(i: Icon, n: Int, s: String) =
    Statistic.simple(TagMod(i.tag, " ", n), s.pluralise(n))

  private def renderLeftContent(p: ProjectCatalogue.Item)(leftContent: TagMod): ReactTag =
    <.div(*.item,
      <.div(*.itemLeft, leftContent),
      <.div(renderStats(p)))

  private def renderMeta(p: ProjectCatalogue.Item): ReactTag =
    <.div(*.itemMeta,
      "Updated ",
      TimeAgo.Component(MomentJs fromInstant p.lastUpdatedOrCreatedAt),
      ".")

  private def renderStats(p: ProjectCatalogue.Item) =
    StatisticGroup.Props(
      statGroupStyle,
      stat(Icon.Write, p.eventCount, "change") ::
        stat(Icon.Cubes, p.reqCount, "req") :: Nil
    ).render

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object AsLink {
    type Props = ProjectCatalogue.Item

    private def render(p: Props): ReactElement =
      renderLeftContent(p) {

        val header =
          <.h1(*.itemHeaderRO,
            <.a(^.href := URLs.PageProject(p.id), p.name))

        header + renderMeta(p)
      }

    val Component = FunctionalComponent(render)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object WithEditableName {

    final case class Props(item           : ProjectCatalogue.Item,
                           state          : ExternalVar[State],
                           asyncFeature   : AsyncActionFeature.D0.Feature[String],
                           renameProjectIO: String => Callback) {
      @inline def render = Component(this)
    }

    // implicit val reusabilityProps: Reusability[Props] =
    //   Reusability.caseClass

    @Lenses
    case class EditState(edit: String, async: AsyncActionFeature.D0.State[String])

    type State = Option[EditState]
    object State {
      private def setFn[A](l: Lens[EditState, A]): A => State => State =
        s => _.map(l set s)

      val setEdit  = setFn(EditState.edit)
      val setAsync = setFn(EditState.async)

      @inline def init: State =
        None
    }

    final class Backend($: BackendScope[Props, Unit]) {

      val inputContMod: TagMod =
        *.itemHeaderEditCont

      val abortFn: Callback =
        $.props.flatMap(_.state set None)

      val updateEditText: String => Callback =
        s => $.props.flatMap(_.state.mod(State setEdit s))

      def renderView(p: Props): TagMod =
        <.h1(*.itemHeaderRW,
          EditTheme.editableInline(p.state set Some(EditState(p.item.name, None))),
          p.item.name
        ) + ProjectItem.renderMeta(p.item)

      def renderEditor(p: Props, s: EditState): TagMod =
        PlainTextEditor.TempBasic.Props.asyncAware(
          text           = s.edit,
          updateText     = updateEditText,
          asyncState     = s.async,
          asyncFeature   = p.asyncFeature,
          nonAsyncStatus = EditorStatus.validate(Validators.projectName)(s.edit, p.renameProjectIO),
          abort          = abortFn,
          inputContMod   = inputContMod)
          .render

      def render(p: Props): ReactElement =
        ProjectItem.renderLeftContent(p.item)(
          p.state.value.fold(renderView(p))(renderEditor(p, _)))
    }

    val Component = ReactComponentB[Props]("ProjectItem")
      .renderBackend[Backend]
      // .configure(Reusability.shouldComponentUpdate)
      .build

  }
}
