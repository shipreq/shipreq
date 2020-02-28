package shipreq.webapp.client.project.app.pages.content.issues

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.data.{Closed, Open}
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.protocol.ManualIssueCmd
import shipreq.webapp.base.text.Text
import shipreq.webapp.base.ui.semantic.{Button, Colour, Icon}
import shipreq.webapp.client.project.app.Style.{issues => *}
import shipreq.webapp.client.project.feature.CreateFeature

object NewIssue {

  final case class Props(state  : StateSnapshot[State],
                         createR: CreateFeature.ReadWrite.ForManualIssueR,
                         createE: CreateFeature.ReadWrite.ForManualIssueE,
                         ) {
    @inline def render: VdomElement = Component(this)
  }

  implicit def reusabilityProps: Reusability[Props] =
    Reusability.when(_.state.value.open is Closed)

  final case class State(open: Open)

  object State {
    def init = State(Closed)

    implicit def reusability: Reusability[State] =
      Reusability.derive
  }

  final class Backend($: BackendScope[Props, Unit]) {

    private def setState(s: State): Callback =
      $.props.flatMap(_.state.setState(s))

    private def newIssueButton(s: State) = {
      val open = Option.when(s.open is Closed)(setState(State(Open)))
      Button(
        tipe = Button.Type.IconAndText(Icon.Plus, "New issue"),
        colour = Colour.Orange,
        state = Button.State.enabledWhen(open.isDefined))
        .tag(^.onClick -->? open)
    }

    private val closeEditor: Callback =
      setState(State(Closed))

    private def save(p: Props, value: Text.ManualIssue.NonEmptyText): Callback = {
      val clearState = p.createR.clearState(CreateFeature.FieldKey.ManualIssue)
      val onSuccess  = closeEditor >> clearState
      p.createR.create(ManualIssueCmd.Create(value), _ => onSuccess)
    }

    def render(p: Props): VdomElement = {

      val editor =
        Option.when(p.state.value.open is Open) {
          val value  = p.createE.value().toOption
          val commit = value.map(save(p, _))
          p.createE.render(CreateFeature.EditorArgs.basic(
            abort  = Some(closeEditor),
            commit = commit))
        }

      <.div(*.newIssueCont,
        <.div(newIssueButton(p.state.value)),
        <.div(*.newIssueForm, editor))
    }

  }

  val Component = ScalaComponent.builder[Props]("NewIssue")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}