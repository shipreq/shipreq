package shipreq.webapp.client.home.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.prefix_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.data.Validators
import shipreq.webapp.base.protocol.InitDataForHomeSpa
import shipreq.webapp.client.base.ui.TextInputAndButton

object Home {
  type Props = InitDataForHomeSpa

  type State = String

  final class Backend($: BackendScope[Props, State]) {

    val updateState: String ~=> Callback =
      ReusableFn($).setState

    def render(p: Props, s: State): ReactElement =
      HomeContent.Props(p, ReusableVar(s)(updateState)).render
  }

  val Component = ReactComponentB[Props]("Home")
    .initialState("")
    .renderBackend[Backend]
    .build
}

object HomeContent {

  final case class Props(data: InitDataForHomeSpa,
                         createProjectText: ReusableVar[String]) {
    @inline def render = Component(this)
  }

  final class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props): ReactElement = {

      val menu = TopMenu.Component(p.data.username)

      val projectCreate = {
        val V = Validators.projectName
        val corrected = V.correctedU(p.createProjectText.value)
        val result =
          if (corrected.value.isEmpty)
            None
          else
            Some(
              V.validateU(corrected).disjunction.bimap(
                _.toText: TagMod,
                n => Callback.alert(s"Creating project: $n")))

        TextInputAndButton.Props(
          p.createProjectText,
          result,
          placeholder = "New  project name...",
          buttonLabel = "Create")
          .render
      }

      val projectList = p.data.projects.items.sortBy(_.name).map(ProjectItem.Component(_))

      <.div(
        menu,
        <.main(Styles.homeContentContainer,
          projectCreate,
          projectList))
    }
  }

  val Component = ReactComponentB[Props]("Home")
    .renderBackend[Backend]
    .build

}
