package shipreq.webapp.client.project.app.issues

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra._
import scalacss.ScalaCssReact._
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.filter.Filter
import shipreq.webapp.base.issue.Issues
import shipreq.webapp.client.project.app.Style.{issues => *}
import shipreq.webapp.client.project.feature.{EditorFeature, RenderFeature}
import shipreq.webapp.client.project.widgets.{FilterEditor, ProjectWidgets}
import shipreq.webapp.base.lib.DataReusability._

object IssuesPage {

  final case class StaticProps(pxProject       : Px[Project],
                               pxRenderFeature : Px[FilterDead => RenderFeature.NoCtx.ForProject],
                               pxProjectWidgets: Px[ProjectWidgets.NoCtx],
                               cmdInvoker      : Action.Cmd ~=> Callback) {

    val pxConfig      = pxProject.map(_.config).withReuse
    val pxFieldNameFn = pxConfig.map(cfg => Reusable.byRef(Field.nameByIdFromProjectConfig(cfg)))

    val component = ScalaComponent.builder[Props]("IssuesPage")
      .backend(new Backend(this, _))
      .renderBackend
      .configure(shouldComponentUpdate)
      .build

    val table = Table.StaticProps(
      pxProject,
      pxRenderFeature.map(_(HideDead)),
      pxProjectWidgets,
      pxFieldNameFn,
      cmdInvoker)
  }

  final case class Props(state   : StateSnapshot[State],
                         editor  : EditorFeature.ReadWrite.ForProject,
                         cmdAsync: AsyncFeature.Read.D1[Action.Cmd, ErrorMsg])

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  final case class State(filterEditor: FilterEditor.State,
                         filterValue: Option[Filter.Valid])

  object State {
    def init = apply(
      filterEditor = FilterEditor.State.init,
      filterValue = None,
    )

    implicit val reusability: Reusability[State] =
      Reusability.byRef || Reusability.derive
  }

  final class Backend(static: StaticProps, $: BackendScope[Props, Unit]) {
    import static.{component => _, _}

    private val filterUpdateFn: FilterEditor.UpdateFn =
      (newState, newValue) =>
        $.props.flatMap(_.state.modState(_.copy(
          filterEditor = newState,
          filterValue = newValue)))

    def render(p: Props): VdomElement = {
      val project = pxProject.value()
      val issues = project.issues
      if (issues.isEmpty)
        renderEmpty
      else
        renderContent(p, issues, project)
    }

    private def renderEmpty =
      <.div(
        NewIssue.render,
        <.div(*.emptyCont, EmptyBody.render))

    private def renderContent(p: Props, issues: Issues, project: Project) = {
      <.div(
        <.div(*.pageRow1,
          <.div(*.pageNew, NewIssue.render),
          <.div(Summary.Props(issues.stats, 0).render)),
        <.div(*.pageRow2,
          // TODO Table config row (sort | filter | cols)
          <.div(*.pageSort),
          <.div(FilterEditor.Props(p.state.value.filterEditor, project, filterUpdateFn).render)),
        table.component(Table.Props(p.editor, p.cmdAsync)))
    }
  }
}