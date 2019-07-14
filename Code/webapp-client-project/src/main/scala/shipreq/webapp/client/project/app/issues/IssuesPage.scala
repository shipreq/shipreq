package shipreq.webapp.client.project.app.issues

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.data._
import shipreq.webapp.base.issue.Issues
import shipreq.webapp.client.project.app.Style.{issues => *}
import shipreq.webapp.client.project.feature.{EditorFeature, RenderFeature}
import shipreq.webapp.client.project.widgets.ProjectWidgets
import shipreq.webapp.base.lib.DataReusability._

object IssuesPage {

  final case class StaticProps(pxProject       : Px[Project],
                               pxRenderFeature : Px[FilterDead => RenderFeature.NoCtx.ForProject],
                               pxProjectWidgets: Px[ProjectWidgets.NoCtx]) {

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
      pxFieldNameFn)
  }

  /*
  state:
  - new editor
  - editor states (shared)
  - table view
    - sort
    - filter
    - column
   */

  final case class Props(editor: EditorFeature.ReadWrite.ForProject)

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  final class Backend(static: StaticProps, $: BackendScope[Props, Unit]) {
    import static.{component => _, _}

    def render(p: Props): VdomElement = {
      val project = pxProject.value()
      val issues = project.issues
      if (issues.isEmpty)
        renderEmpty
      else
        renderContent(p, issues)
    }

    private def renderEmpty =
      <.div(
        NewIssue.render,
        <.div(*.emptyCont, EmptyBody.render))

    private def renderContent(p: Props, issues: Issues) = {
      <.div(*.pageCont,
        <.div(*.pageNew, NewIssue.render),
        <.div(*.pageSummary, Summary.Props(issues.stats, 0).render),
        // TODO Table config row (sort | filter | cols)
        <.div(*.pageTable, table.component(Table.Props(p.editor))))
    }
  }
}