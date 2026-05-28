package shipreq.webapp.client.project.app.pages.content.issues

import japgolly.scalajs.react.ReactMonocle._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import scalacss.ScalaCssReact._
import shipreq.base.util.{ErrorMsg, Permission}
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.client.project.app.Style.{issues => *}
import shipreq.webapp.client.project.app.pages.root.Routes
import shipreq.webapp.client.project.feature.create.Feature.PreviewId
import shipreq.webapp.client.project.feature.{CreateFeature, EditorFeature}
import shipreq.webapp.client.project.widgets.{FilterEditor, ProjectWidgets}
import shipreq.webapp.member.feature.PreviewFeature
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.filter.{CompiledFilter, Filter}
import shipreq.webapp.member.project.issue.Issues
import shipreq.webapp.member.project.text.{PlainText, TextSearch}
import shipreq.webapp.member.project.util.DataReusability._

object IssuesPage {

  final case class StaticProps(pxProject       : Px[Project],
                               pxRenderFeature : Px[FilterDead => RenderFeature.ForProject],
                               pxPlainText     : Px[PlainText.ForProject.NoCtx],
                               pxProjectWidgets: Px[ProjectWidgets.NoCtx],
                               pxTextSearch    : Px[TextSearch],
                               pxFilterCompiler: Px[Filter.Valid.Compiler],
                               routerCtl       : Routes.RouterCtl,
                               cmdInvoker      : Action.Cmd ~=> Callback) {

    val pxConfig      = pxProject.map(_.config).withReuse
    val pxFieldNameFn = pxConfig.map(cfg => Reusable.byRef(cfg.fieldName))

    val component = ScalaComponent.builder[Props]
      .backend(new Backend(this, _))
      .renderBackend
      .build

    val table = Table.StaticProps(
      pxProject,
      pxRenderFeature.map(_(HideDead)),
      pxPlainText,
      pxProjectWidgets,
      pxFieldNameFn,
      routerCtl,
      cmdInvoker)
  }

  final case class Props(state      : StateSnapshot[State],
                         creator    : CreateFeature.ReadWrite.ForManualIssueR,
                         editor     : EditorFeature.ReadWrite.ForProject,
                         editorArgs : EditorFeature.EditorArgs.ForAny,
                         previewRW  : PreviewFeature.ReadWrite.Composite[PreviewId],
                         cmdAsync   : AsyncFeature.Read.D1[Action.Cmd, ErrorMsg],
                         editability: Permission)

  @Lenses
  final case class State(newIssue    : NewIssue.State,
                         filterEditor: FilterEditor.State,
                         filterValue : Option[Filter.Valid])

  object State {
    def init = apply(
      newIssue     = NewIssue.State.init,
      filterEditor = FilterEditor.State.init,
      filterValue  = None,
    )

    implicit val reusability: Reusability[State] =
      Reusability.derive
  }

  final class Backend(static: StaticProps, $: BackendScope[Props, Unit]) {
    import static.{component => _, _}

    private val pxFilterValid: Px[Option[Filter.Valid]] =
      Px.props($).map(_.state.value.filterValue).withReuse.autoRefresh

    private val pxFilterCompiled: Px[Option[CompiledFilter]] =
      for {
        c <- pxFilterCompiler
        f <- pxFilterValid
      } yield f.map(c)

    private val pxFilteredIssues: Px[Issues] =
      for {
        p <- pxProject
        f <- pxFilterCompiled
      } yield f.fold(p.issues)(p.issues.filter)

    private val filterUpdateFn: FilterEditor.UpdateFn =
      (newState, newValue, cb) =>
        $.props.flatMap(_.state.modState(
          _.copy(
            filterEditor = newState,
            filterValue = newValue),
          cb))

    def render(p: Props): VdomElement = {
      val project        = pxProject.value()
      def projectWidgets = pxProjectWidgets.value()
      def textSearch     = pxTextSearch.value()

      def renderNew =
        NewIssue.Props(
          previewRW      = p.previewRW,
          project        = project,
          textSearch     = textSearch,
          projectWidgets = projectWidgets,
          state          = p.state.zoomStateL(State.newIssue),
          createR        = p.creator,
        ).render

      def renderEmpty =
        <.div(
          renderNew,
          <.div(*.emptyCont, NoContent.render))

      def renderContent(issues: Issues) = {
        val filteredOut = project.issues.vector.length - issues.vector.length

        <.div(
          <.div(*.pageRow1,
            <.div(*.pageNew, renderNew),
            <.div(Summary.Props(issues.stats, filteredOut).render)),
          <.div(*.pageRow2,
            <.div(*.pageSort),
            <.div(FilterEditor.Props(p.state.value.filterEditor, project, filterUpdateFn).render)),
          table.component(Table.Props(issues, p.editor, p.editorArgs, p.cmdAsync, p.editability)))
      }

      val issues = project.issues
      if (issues.isEmpty)
        renderEmpty
      else
        renderContent(pxFilteredIssues.value())
    }
  }
}
