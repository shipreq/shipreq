package shipreq.webapp.client.project.app.pages.admin.access

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.base.util._
import shipreq.webapp.base.data._
import shipreq.webapp.base.feature.AsyncFeature
import shipreq.webapp.base.lib.ConfirmJs
import shipreq.webapp.base.protocol.ServerSideProcInvoker
import shipreq.webapp.base.ui.semantic.{Button, Colour, ColourPlus, Header, Icon, Segment}
import shipreq.webapp.client.project.app.Style.accessPage.{existingUserSegment => *}
import shipreq.webapp.member.project.data.{Colour => _, _}
import shipreq.webapp.member.project.protocol.websocket.UpdateAccessCmd
import shipreq.webapp.member.project.util.DataReusability._

object ExistingUserSegment {

  final case class Props(userId         : UserId.Public,
                         access         : ProjectAccess,
                         rolodex        : Rolodex,
                         editability    : Permission,
                         state          : StateSnapshot[State],
                         confirmJs      : ConfirmJs,
                         sspUpdateAccess: ServerSideProcInvoker[UpdateAccessCmd.Modify, ErrorMsg, Any],
                         async          : AsyncFeature.ReadWrite.D1[AsyncKey, ErrorMsg]) {
    @inline def render: VdomElement = Component(this)
  }

  object Props {
    implicit val reusability: Reusability[Props] =
      Reusability.derive
  }

  type State = Map[UserId.Public, ProjectPerm]

  object State {
    implicit val reusability: Reusability[State] =
      Reusability.byUnivEq

    def init: State =
      Map.empty
  }

  // ===================================================================================================================

  def render(p: Props) = {
    val s = p.state.value

    def row(name: String, id: UserId.Public, delete: Permission) = {

      val asyncKey =
        AsyncKey(id)

      val inFlight: Boolean =
        p.async(asyncKey).isInProgress

      val saved: ProjectPerm =
        // Not using .need here because we don't want to crash between a user leaing the project, and them being
        // redirected to the access-revoked page.
        p.access(id).getOrElse(ProjectPerm.min)

      val selected: ProjectPerm =
        p.editability match {
          case Allow => s.get(id).getOrElse(saved)
          case Deny  => saved
        }

      def setSelected(selected: ProjectPerm): State => State =
        if (selected ==* saved)
          _ - id
        else
          _.updated(id, selected)

      val applyButton = {
        val button = Button(
          tipe   = Button.Type.IconAndText(Icon.Edit, "Save"),
          state  = Button.State.loadingWhen(inFlight),
          colour = Colour.Green,
        )
        val cmd = UpdateAccessCmd.Modify(Map(id -> Some(selected)))
        val apply = p.async(asyncKey).write.onFailureShowAndForget(p.sspUpdateAccess(cmd))
        val tag = button.onClick(apply)

        tag(^.visibility.hidden.when(selected ==* saved))
      }

      val deleteButton = {
        val button = Button(
          tipe   = Button.Type.BasicIconOnly(Icon.Trash),
          state  = Button.State.loadingWhen(inFlight),
          colour = ColourPlus.Negative,
        )
        val cmd = UpdateAccessCmd.Modify(Map(id -> None))
        val apply =
        for {
          proceed <- p.confirmJs(s"Are you sure you want to revoke $name's access?")
          _ <- p.async(asyncKey).write.onFailureShowAndForget(p.sspUpdateAccess(cmd)).when(proceed)
        } yield ()
        val tag = button.onClick(apply)

        tag(^.visibility.hidden.when(delete.is(Deny) || p.editability.is(Deny)))
      }

      val dropdown =
        PermDropdown(
          selected = selected,
          enabled  = Enabled.when(p.editability.is(Allow) && !inFlight),
          onChange = i => p.state.modState(setSelected(i.value)))

      <.tr(
        ^.key := name,
        <.td(Icon.User.tag, name),
        <.td(*.tableCellDropdown, dropdown.render),
        <.td(*.tableCellApply, applyButton),
        <.td(*.tableCellDelete, deleteButton),
      )
    }

    val self = row("You", p.userId, Deny)

    val others =
      MutableArray(p.access.asMap.iterator.map(e => (e._1, p.rolodex.need(e._1))))
        .sortBy(_._2.value)
        .iterator()
        .filterNot(_._1 ==* p.userId)
        .toVdomArray { case (id, username) => row(username.with_@, id, Allow) }

    Segment.raised(*.segment,
      Header.h4("Existing Users"),
      <.table(*.table,
        <.tbody(
          self,
          others)))
  }

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .configure(Reusability.shouldComponentUpdate)
    .build
}
