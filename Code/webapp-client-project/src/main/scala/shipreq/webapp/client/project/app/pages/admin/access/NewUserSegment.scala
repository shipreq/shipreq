package shipreq.webapp.client.project.app.pages.admin.access

import japgolly.scalajs.react.ReactMonocle._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import scalacss.ScalaCssReact._
import shipreq.base.util.{Deny, Disabled, Enabled, ErrorMsg, Permission}
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.ui.CommmonUiText
import shipreq.webapp.base.ui.semantic.{Button, Colour, Header, Icon, Segment, UsesSemanticUiManually}
import shipreq.webapp.base.ui.widgets.Form
import shipreq.webapp.base.validation.lib.{Composite, Simple}
import shipreq.webapp.base.validation.{UserValidators, ValidationUX}
import shipreq.webapp.client.project.app.Style.accessPage.{newUserSegment => *}
import shipreq.webapp.member.project.protocol.websocket.UpdateAccessCmd
import shipreq.webapp.member.project.util.DataReusability._

object NewUserSegment {

  final case class Props(state          : StateSnapshot[State],
                         editability    : Permission,
                         sspUpdateAccess: ServerSideProcInvoker[UpdateAccessCmd.Add, ErrorMsg, Any],
                         async          : AsyncFeature.ReadWrite.D0[ErrorMsg]) {
    @inline def render: VdomElement = Component(this)
  }

  object Props {
    implicit val reusability: Reusability[Props] =
      Reusability.derive
  }

  @Lenses
  final case class State(user: String,
                         perm: ProjectPerm)

  object State {
    implicit val reusability: Reusability[State] =
      Reusability.derive

    def init: State =
      State("", ProjectPerm.Collaborator)
  }

  // ===================================================================================================================

  // No need to display error messages for invalid username/email; the Add button being disabled when invalid is enough.
  private implicit def vux = ValidationUX.Off

  private val userValidator: Simple.Validator[String, String, Username \/ EmailAddr] =
    UserValidators.usernameOrEmail.mapError(Composite.Invalidity.toLines(_).toNES)

  @UsesSemanticUiManually
  private def render(p: Props) = {

    val inFlight: Boolean =
      p.async.isInProgress

    val enabled: Enabled =
      Disabled.when(inFlight || p.editability.is(Deny))

    val fieldUserOrEmail: Form.Field[String] =
      Form.Field.text
        .withLabel(CommmonUiText.usernameOrEmail)
        .withState(p.state.zoomStateL(State.user))
        .withValidator(userValidator) // used here only for the input correction
        .withEnabled(enabled)
        .withOuterMod(_(*.fieldUser))

    val permDropdown = PermDropdown(
      selected = p.state.value.perm,
      enabled  = enabled,
      onChange = item => p.state.setStateL(State.perm)(item.value)
    ).render

    val fieldPerm: Form.Field[Unit] =
      Form.Field.ofEditor(permDropdown)
        .withLabel("Role")
        .withEnabled(enabled)
        .withOuterMod(_(*.fieldPerm))

    val onAdd: Option[Callback] =
      enabled match {
        case Enabled => userValidator(p.state.value.user).toOption.map { userOrEmail =>
          val cmd = UpdateAccessCmd.Add(userOrEmail, p.state.value.perm)
          val run = p.sspUpdateAccess(cmd).flatTap {
            case \/-(_) => p.state.setState(State.init).asAsyncCallback // reset form on success
            case -\/(_) => AsyncCallback.unit
          }
          p.async.write.onFailureShowAndForget(run)
        }
        case Disabled => None
      }

    val addButton = {
      val button = Button(
        tipe   = Button.Type.IconAndText(Icon.Plus, "Add"),
        state  = Button.State.loadingOrEnabled(loading = inFlight, enabled = onAdd.isDefined),
        colour = Colour.Green,
      )
      button.onClick(onAdd.getOrElse(Callback.empty))
    }

    val fieldAdd =
      Form.Field.ofEditor(addButton)
        .withOuterMod(_(*.fieldAdd))

    val form =
      <.div(^.className := "ui form",
        <.div(^.className := "fields",
          fieldUserOrEmail.render,
          fieldPerm.render),
        fieldAdd.render)

    Segment.raised(*.segment,
      Header.h4("New User"),
      form)
  }

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}
