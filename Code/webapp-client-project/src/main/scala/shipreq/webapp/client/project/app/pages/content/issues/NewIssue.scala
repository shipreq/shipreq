package shipreq.webapp.client.project.app.pages.content.issues

import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.StateSnapshot
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.ui.semantic.{Button, Colour, Icon}
import shipreq.webapp.base.util._
import shipreq.webapp.client.project.app.Style.{issues => *}
import shipreq.webapp.client.project.feature.CreateFeature
import shipreq.webapp.client.project.feature.create.Feature.PreviewId
import shipreq.webapp.client.project.widgets.ProjectWidgets
import shipreq.webapp.member.feature.PreviewFeature
import shipreq.webapp.member.project.data._
import shipreq.webapp.member.project.protocol.websocket.ManualIssueCmd
import shipreq.webapp.member.project.text.{Text, TextSearch}
import shipreq.webapp.member.project.util.DataReusability._

object NewIssue {

  final case class Props(previewRW     : PreviewFeature.ReadWrite.Composite[PreviewId],
                         project       : Project,
                         textSearch    : TextSearch,
                         projectWidgets: ProjectWidgets.NoCtx,
                         state         : StateSnapshot[State],
                         createR       : CreateFeature.ReadWrite.ForManualIssueR,
                         ) {

    val createE: Option[CreateFeature.ReadWrite.ForManualIssueE] =
      createR(CreateFeature.FieldKey.ManualIssue).toOption

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

    private def newIssueButton(p: Props) = {
      val s = p.state.value
      val open = Option.when(s.open.is(Closed) && p.createE.isDefined)(setState(State(Open)))
      Button(
        tipe = Button.Type.IconAndText(Icon.Plus, "New issue"),
        colour = Colour.Orange,
        state = Button.State.enabledWhen(open.isDefined))
        .tag(^.onClick -->? open)
    }

    private val closeEditor: Reusable[Callback] =
      Reusable.callbackByRef(setState(State(Closed)))

    private def editorValue(p: Props, createE: CreateFeature.ReadWrite.ForManualIssueE): Option[Text.ManualIssue.NonEmptyText] = {
      val emptyArgs =
        CreateFeature.EditorArgs.ForTextEditor.empty(
          project        = p.project,
          textSearch     = p.textSearch,
          projectWidgets = p.projectWidgets)

      createE.value(emptyArgs).toOption
    }

    private def save(p: Props, value: Text.ManualIssue.NonEmptyText): Callback = {
      val clearState = p.createR.clearState(CreateFeature.FieldKey.ManualIssue)
      val onSuccess  = closeEditor >> clearState
      p.createR.create(ManualIssueCmd.Create(value), _ => onSuccess)
    }

    private val commit: Reusable[Text.ManualIssue.NonEmptyText => Callback] =
      Reusable.byRef(newValue =>
        for {
          p <- $.props
          _ <- save(p, newValue)
        } yield ()
      )

    def render(p: Props): VdomElement = {

      val editor: Option[VdomElement] =
        if (p.state.value.open is Closed)
          None
        else
          p.createE.map { createE =>
            val value = editorValue(p, createE)
            val commit = value.map(_ => this.commit)

            val args =
              CreateFeature.EditorArgs.ForTextEditor.basic(
                previewRW      = p.previewRW,
                project        = p.project,
                textSearch     = p.textSearch,
                projectWidgets = p.projectWidgets,
                abort          = Some(closeEditor),
                commit         = commit)

            createE.render(args)
          }

      <.div(*.newIssueCont,
        <.div(newIssueButton(p)),
        <.div(*.newIssueForm, editor))
    }

  }

  val Component = ScalaComponent.builder[Props]
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build
}
