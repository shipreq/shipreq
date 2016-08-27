package shipreq.webapp.client.home.ui

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._
import monocle.macros.Lenses
import scalacss.ScalaCssReact._
import shipreq.webapp.base.data.{ProjectCatalogue, Username, Validators}
import shipreq.webapp.base.protocol.InitDataForHomeSpa
import shipreq.webapp.client.base.ClientConfig
import shipreq.webapp.client.base.feature.{AsyncActionFeature, EditorStatus}
import shipreq.webapp.client.base.protocol.ClientProtocol
import shipreq.webapp.client.base.ui.{BaseStyles, MemberNavBar, PlainTextEditor, ProjectItem}
import shipreq.webapp.client.base.ui.semantic.Breadcrumb

object Home {
  case class Props(data: InitDataForHomeSpa, cp: ClientProtocol) {
    @inline def render = Component(this)
  }

  @Lenses
  case class State(createProjectText: String,
                   createProjectAAS : AsyncActionFeature.D0.State[String],
                   projects         : ProjectCatalogue)

  final class Backend($: BackendScope[Props, State]) {

    val setCreateProjectText: String ~=> Callback =
      ReusableFn($ zoomL State.createProjectText).setState

    val createProjectAAF =
      AsyncActionFeature.D0.Feature[String]($ zoomL State.createProjectAAS)

    def addProject(i: ProjectCatalogue.Item): Callback =
      $.modState(State.projects.modify(p => ProjectCatalogue(p.items :+ i)))

    val createProjectIO: String => Callback =
      name =>
        $.props >>= (p =>
          createProjectAAF.wrapAsync((onSuccess, onFailure) =>
            p.cp.call(p.data.createProject)(
              name,
              i => onSuccess >> setCreateProjectText("") >> addProject(i),
              _ consumeAnd onFailure)))

    def render(p: Props, s: State): ReactElement =
      HomeContent.Props(
        p.data.username,
        s.projects,
        ReusableVar(s.createProjectText)(setCreateProjectText),
        createProjectAAF,
        s.createProjectAAS,
        createProjectIO)
        .render
  }

  val Component = ReactComponentB[Props]("Home")
    .initialState_P(p => State("", AsyncActionFeature.D0.initState, p.data.projects))
    .renderBackend[Backend]
    .build
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object HomeContent {

  final case class Props(username         : Username,
                         projects         : ProjectCatalogue,
                         createProjectText: ReusableVar[String],
                         createProjectAF  : AsyncActionFeature.D0.Feature[String],
                         createProjectAS  : AsyncActionFeature.D0.State[String],
                         createProjectIO  : String => Callback) {
    @inline def render = Component(this)
  }

  final class Backend($: BackendScope[Props, Unit]) {

    val navBarLeft =
      Breadcrumb.Item.Div(ClientConfig.BreadcrumbNameMemberHome) :: Nil

    val inputMod: TagMod =
      ^.placeholder := "New project name..."

    def render(p: Props): ReactElement = {

      val menu = MemberNavBar.Props(p.username, navBarLeft, Nil).render

      val projectCreate = {
        val status =
          EditorStatus.async(p.createProjectAS, p.createProjectAF) getOrElse
            EditorStatus.ignoreOrValidate(Validators.projectName)(
              p.createProjectText.value, _.isEmpty, p.createProjectIO)

        PlainTextEditor.WithButton.Props(
          p.createProjectText.value,
          p.createProjectText.set,
          status,
          buttonLabel = "Create",
          inputMod = inputMod)
          .render
      }

      val projectList = p.projects.items.sortBy(_.name).map(ProjectItem.AsLink.Component(_))

      <.div(
        menu,
        <.main(BaseStyles.containerLarge,
          <.div(Styles.createProjectContainer, projectCreate),
          projectList))
    }
  }

  val Component = ReactComponentB[Props]("Home")
    .renderBackend[Backend]
    .build

}
