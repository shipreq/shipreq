package shipreq.webapp.client.home.ui

import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import scalacss.ScalaCssReact._
import shipreq.base.util.ErrorMsg
import shipreq.webapp.base.config.{ClientConfig, WebappConfig}
import shipreq.webapp.base.feature.{AsyncFeature, EditorStatus, ErrorHandlingFeature}
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.protocol.ajax.{AjaxClient, CommonProtocolsJs}
import shipreq.webapp.base.ui.semantic.{Breadcrumb, Colour}
import shipreq.webapp.base.ui.widgets._
import shipreq.webapp.base.util.CallbackHelpers._
import shipreq.webapp.member.project.data.{DataValidators, ProjectMetaData}
import shipreq.webapp.member.protocol.ajax.HomeSpaProtocols
import shipreq.webapp.member.protocol.entrypoint.HomeSpaEntryPoint
import shipreq.webapp.member.ui._

object Home {
  final case class Props(data: HomeSpaEntryPoint.InitData,
                         ajax: AjaxClient.Binary) {
    @inline def render = Component(this)

    def createProjectIO: ServerSideProcInvoker[String, ErrorMsg, ProjectMetaData] =
      ajax.invoker(HomeSpaProtocols.CreateProject.ajax)

    val feedbackModal: FeedbackModal = {
      val metadata = CommonProtocolsJs.Metadata.client(data.username)
      FeedbackModal(metadata, ajax)
    }
  }

  @Lenses
  final case class State(createProjectText: String,
                         createProjectAAS : AsyncFeature.Read.D0[ErrorMsg],
                         projects         : List[ProjectMetaData])

  object State {

    val recorder = ErrorHandlingFeature.StateRecorder[State]

    def init(projects: List[ProjectMetaData]): State =
      State(
        createProjectText = "",
        createProjectAAS  = AsyncFeature.State.initD0,
        projects          = projects,
      )
  }

  final class Backend($: BackendScope[Props, State]) {

    val setCreateProjectText: Reusable[SetStateFnPure[String]] =
      Reusable.fn.state($ zoomStateL State.createProjectText).setStateFn

    val createProjectAF: AsyncFeature.Write.D0[ErrorMsg] =
      AsyncFeature.Write.D0.init($ zoomStateL State.createProjectAAS)

    def addProject(i: ProjectMetaData): Callback =
      $.modState(State.projects.modify(_ :+ i))

    val createProjectIO: String => Callback =
      name =>
        $.props.flatMap(p =>
          createProjectAF(
            p.createProjectIO(name)
              .rightFlatTapSync(i => setCreateProjectText.setState("") >> addProject(i))
          )
        )

    val navBarLeft: MemberNavBar.LeftProps =
      Reusable.byRef(Breadcrumb.Item.Section(ClientConfig.BreadcrumbNameMemberHome) :: Nil)

    def render(p: Props, s: State): VdomElement = {
      State.recorder.record(s)

      val navBar = MemberNavBar.Props(p.data.username, Some(p.feedbackModal), p.data.assetManifest, navBarLeft)

      def mainContent(m: TagMod): VdomElement =
        HomeContent.Props(
          s.projects,
          StateSnapshot.withReuse(s.createProjectText)(setCreateProjectText),
          s.createProjectAAS,
          createProjectIO,
          m)
          .render

      <.div(
        p.feedbackModal.render,
        MemberLayout.Props(navBar, mainContent).render)
    }
  }

  val Component = ScalaComponent.builder[Props]
    .initialStateCallbackFromProps(p => State.recorder.getOrElse(State.init(p.data.projects)))
    .renderBackend[Backend]
    .build
}

// ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

object HomeContent {

  final case class Props(projects         : List[ProjectMetaData],
                         createProjectText: StateSnapshot[String],
                         createProjectAS  : AsyncFeature.Read.D0[ErrorMsg],
                         createProjectIO  : String => Callback,
                         tagMod           : TagMod) {
    @inline def render = Component(this)
  }

  final class Backend {

    val inputMod: TagMod =
      TagMod(^.placeholder := "New project name", Styles.createProjectInput)

    def render(p: Props): VdomElement = {

      val noProjects = p.projects.isEmpty

      val createProject = {
        val status: EditorStatus =
          EditorStatus.async(p.createProjectAS) getOrElse
            EditorStatus.ignoreOrValidate(DataValidators.projectName.unnamed)(
              p.createProjectText.value, _.isEmpty, s => Some(p.createProjectIO(s)))
        <.div(Styles.createProjectCont,
          PlainTextEditor.WithButton.Props(
            p.createProjectText.value,
            p.createProjectText.setState,
            status,
            Colour.Green,
            buttonLabel = "Create Project",
            inputMod = TagMod(inputMod, (^.autoFocus := true).when(noProjects)))
            .render)
      }

      def noProjectGreeting: VdomTag =
        <.div(Styles.noProjects,
          NoContentMessage(
            s"Welcome to ${WebappConfig.appName}!",
            TagMod(
              "The first thing you'll want to do is create a project to contain all of your requirements.",
              <.br,
              "Create a new project using the button above.")))

      def projectList: VdomTag =
        <.div(Styles.projectList,
          p.projects.sortBy(_.name).toTagMod(ProjectItem.AsLink.Component(_)))

      <.main(
        BaseStyles.containerLarge,
        p.tagMod,
        createProject,
        if (noProjects) noProjectGreeting else projectList)
    }
  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .build

}
