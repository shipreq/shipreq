package shipreq.webapp.client.project.app.root

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.data.ProjectMetaData
import shipreq.webapp.base.user.Username
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.ui._
import shipreq.webapp.base.ui.semantic.{Breadcrumb, Colour, Icon, Menu}
import shipreq.webapp.client.project.app.Style
import shipreq.webapp.client.project.widgets.{FilterHelp, RichTextEditorHelp}
import Routes.{Page, RouterCtl}

object Layout {

  final case class Props(username        : Username,
                         project         : ProjectMetaData,
                         connectionStatus: ConnectionStatus,
                         reauthModal     : ReauthenticationModal,
                         rc              : RouterCtl,
                         page            : Page,
                         content         : VdomElement) {
    @inline def render = Component(this)
  }

  // -------------------------------------------------------------------------------------------------------------------

  private type NavBarLeftInput = (Page, ProjectMetaData, RouterCtl)

  private val reusabilityNavBarLeftInput: Reusability[NavBarLeftInput] =
    implicitly[Reusability[NavBarLeftInput]]

  private def navBarLeft(input: NavBarLeftInput): MemberNavBar.LeftProps =
    Reusable.explicitly(input)(reusabilityNavBarLeftInput).map { case (page, project, rc) =>

      def index = Breadcrumb.Item.Link(rc.link(Page.Index)(project.name))

      val tail = page match {
        case Page.Index =>
          Breadcrumb.Item.Div(project.name) :: Nil

        case Page.ReqDetail(id) =>
          val menuItems  = ProjectIndex.dropdownItems(None, rc)
          val reqTable   = rc.link(Page.ReqTable)(ProjectIndex.Item.ReqTable.iconAndTitle)
          val menu       = Breadcrumb.Item.LinkAndDropdown(reqTable, menuItems)
          val reqLabel   = <.span(ProjectIndex.Item.ReqDetail.icon.tag, " " + PlainText.pubid(id))
          val req        = Breadcrumb.Item.Div(reqLabel, Breadcrumb.ItemState.Active)
          index :: menu :: req :: Nil

        case p =>
          val activeItem = ProjectIndex.Item.ToPage.reverse getOption p getOrElse sys.error("No breadcrumb menu item for " + p)
          val menuItems  = ProjectIndex.dropdownItems(Some(activeItem), rc)
          val menu       = Breadcrumb.Item.DropDown(activeItem.iconAndTitle, menuItems)
          index :: menu :: Nil
      }

      MemberNavBar.MemberHome :: tail
    }

  // -------------------------------------------------------------------------------------------------------------------

  private type NavBarRightInput = ConnectionStatus

  private val reusabilityNavBarRightInput: Reusability[NavBarRightInput] =
    implicitly[Reusability[NavBarRightInput]]

  private val connectedMenuItem =
    ConnectionStatus.memo { c =>
      val icon = c match {
        case ConnectionStatus.Connected =>
          Icon.Plug.withColour(Colour.Green).tag(Style.navBar.connected, ^.title := "connected")

        case ConnectionStatus.Disconnected =>
          Icon.Plug.withColour(Colour.Red).tag(Style.navBar.disconnected, ^.title := "disconnected")
      }

      Menu.Item(Menu.ItemType.Div(icon))
    }

  private def navBarRight(input: NavBarRightInput): MemberNavBar.RightProps =
    Reusable.explicitly(input)(reusabilityNavBarRightInput).map { connectionStatus =>
      connectedMenuItem(connectionStatus) :: Nil
    }

  // -------------------------------------------------------------------------------------------------------------------

  private def render(p: Props): VdomElement = {
    val menuLeft  = navBarLeft((p.page, p.project, p.rc))
    val menuRight = navBarRight(p.connectionStatus)
    MemberLayout.Props(
      MemberNavBar.Props(p.username, menuLeft, menuRight),
      <.div(
        _,
        Style.layout,
        FilterHelp.modal.render,
        p.reauthModal.render,
        RichTextEditorHelp.allRendered,
        p.content))
      .render
  }

  val Component = ScalaFnComponent(render)
}
