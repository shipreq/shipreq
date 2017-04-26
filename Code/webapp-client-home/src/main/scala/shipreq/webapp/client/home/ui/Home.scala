package shipreq.webapp.client.home.ui

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import scalacss.ScalaCssReact._
import shipreq.webapp.base.data.{ProjectCatalogue, Username, DataValidators}
import shipreq.webapp.base.protocol.InitDataForHomeSpa
import shipreq.webapp.client.base.ClientConfig
import shipreq.webapp.client.base.feature.{AsyncFeature, EditorStatus}
import shipreq.webapp.client.base.protocol.ClientProtocol
import shipreq.webapp.client.base.ui.{BaseStyles, MemberNavBar, PlainTextEditor, ProjectItem}
import shipreq.webapp.client.base.ui.semantic.Breadcrumb

object Home {
  final case class Props(data: InitDataForHomeSpa, cp: ClientProtocol) {
    @inline def render = Component(this)
  }

  @Lenses
  final case class State(createProjectText: String,
                         createProjectAAS : AsyncFeature.ReadOnly.D0[String],
                         projects         : ProjectCatalogue)

  final class Backend($: BackendScope[Props, State]) {

    val setCreateProjectText: String ~=> Callback =
      Reusable.fn.state($ zoomStateL State.createProjectText).set

    val createProjectAF: AsyncFeature.Feature.D0[String] =
      AsyncFeature.Feature.D0.init($ zoomStateL State.createProjectAAS)

    def addProject(i: ProjectCatalogue.Item): Callback =
      $.modState(State.projects.modify(p => ProjectCatalogue(p.items :+ i)))

    val createProjectIO: String => Callback =
      name =>
        $.props >>= (p =>
          createProjectAF((onSuccess, onFailure) =>
            p.cp.call(p.data.createProject)(
              name,
              i => onSuccess >> setCreateProjectText("") >> addProject(i),
              _ consumeAnd onFailure)))

    def render(p: Props, s: State): VdomElement =
      HomeContent.Props(
        p.data.username,
        s.projects,
        StateSnapshot.withReuse(s.createProjectText)(setCreateProjectText),
        s.createProjectAAS,
        createProjectIO)
        .render
  }

  val Component = ScalaComponent.builder[Props]("Home")
    .initialStateFromProps(p => State("", AsyncFeature.State.initD0, p.data.projects))
    .renderBackend[Backend]
    .build
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object HomeContent {

  final case class Props(username         : Username,
                         projects         : ProjectCatalogue,
                         createProjectText: StateSnapshot[String],
                         createProjectAS  : AsyncFeature.ReadOnly.D0[String],
                         createProjectIO  : String => Callback) {
    @inline def render = Component(this)
  }

  final class Backend($: BackendScope[Props, Unit]) {

    val navBarLeft =
      Breadcrumb.Item.Div(ClientConfig.BreadcrumbNameMemberHome) :: Nil

    val inputMod: TagMod =
      ^.placeholder := "New project name..."

    def render(p: Props): VdomElement = {

      val menu = MemberNavBar.Props(p.username, navBarLeft, Nil).render

      val projectCreate = {
        val status =
          EditorStatus.async(p.createProjectAS) getOrElse
            EditorStatus.ignoreOrValidate(DataValidators.projectName.unnamed)(
              p.createProjectText.value, _.isEmpty, p.createProjectIO)

        PlainTextEditor.WithButton.Props(
          p.createProjectText.value,
          p.createProjectText.setState,
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
          projectList.toTagMod))
    }
  }

  val Component = ScalaComponent.builder[Props]("Home")
    .renderBackend[Backend]
    .build

}
