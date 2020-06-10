package shipreq.webapp.base.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Lens
import monocle.macros.Lenses
import scalacss.ScalaCssReact._
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.UiText.EnglishStringExt
import shipreq.webapp.base.Urls
import shipreq.webapp.base.data.{DataValidators, ProjectMetaData}
import shipreq.webapp.base.feature.{AsyncFeature, EditorStatus}
import shipreq.webapp.base.jsfacade.MomentJs
import shipreq.webapp.base.ui.BaseStyles.{projectItems => *}
import shipreq.webapp.base.ui.semantic.{Icon, Size, Statistic, StatisticGroup}

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

  private def renderLeftContent(p: ProjectMetaData)(leftContent: TagMod): VdomTag =
    <.div(*.item,
      <.div(*.itemLeft, leftContent),
      <.div(renderStats(p)))

  private def renderMeta(p: ProjectMetaData): VdomTag =
    <.div(*.itemMeta,
      "Updated ",
      TimeAgo.Component(MomentJs fromInstant p.lastUpdatedOrCreatedAt),
      ".")

  private def renderStats(p: ProjectMetaData) =
    StatisticGroup.Props(
      statGroupStyle,
      stat(Icon.Write, p.eventsPostInit, "change") ::
        stat(Icon.Cubes, p.reqsLive, "req") :: Nil
    ).render

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object AsLink {
    type Props = ProjectMetaData

    private def render(p: Props): VdomElement =
      renderLeftContent(p) {

        val header =
          <.h1(*.itemHeaderRO,
            <.a(^.href := Urls.project(p.id).relativeUrl, p.name))

        TagMod(header, renderMeta(p))
      }

    val Component = ScalaFnComponent(render)
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

  object WithEditableName {

    final case class Props(item           : ProjectMetaData,
                           state          : StateSnapshot[State],
                           renameProjectIO: String => Callback) {
      @inline def render = Component(this)
    }

    // implicit val reusabilityProps: Reusability[Props] =
    //   Reusability.derive

    @Lenses
    final case class EditState(edit: String, async: AsyncFeature.State.D0[ErrorMsg]) {

      def corrected: String =
        DataValidators.projectName.unnamed.corrector.full(edit)
    }

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
        $.props.flatMap(_.state setState None)

      val updateEditText: String => Callback =
        s => $.props.flatMap(_.state.modState(State setEdit s))

      def renderView(p: Props): TagMod =
        TagMod(
          <.h1(*.itemHeaderRW,
            EditTheme.editableInline(p.state setState Some(EditState(p.item.name, None))),
            p.item.name),
          ProjectItem.renderMeta(p.item))

      def renderEditor(p: Props, s: EditState): TagMod = {
        val status =
          EditorStatus.async(s.async) getOrElse
            EditorStatus.validate(DataValidators.projectName.unnamed)(s.edit, s => Some(p.renameProjectIO(s)))

        PlainTextEditor.TempBasic.Props(
          text         = s.edit,
          updateText   = updateEditText,
          status       = status,
          abort        = Some(abortFn),
          inputContMod = inputContMod)
          .render
      }

      def render(p: Props): VdomElement =
        ProjectItem.renderLeftContent(p.item)(
          p.state.value.fold(renderView(p))(renderEditor(p, _)))
    }

    val Component = ScalaComponent.builder[Props]
      .renderBackend[Backend]
      // .configure(Reusability.shouldComponentUpdate)
      .build

  }
}
