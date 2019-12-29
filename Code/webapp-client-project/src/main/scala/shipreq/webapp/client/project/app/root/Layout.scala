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

  final case class Props(username           : Username,
                         project            : ProjectMetaData,
                         connectionStatus   : ConnectionStatus,
                         setConnectionStatus: ConnectionStatus => Reusable[Callback],
                         reauthModal        : ReauthenticationModal,
                         rc                 : RouterCtl,
                         page               : Page,
                         content            : VdomElement) {
    @inline def render = Component(this)
  }

  // -------------------------------------------------------------------------------------------------------------------

  private final case class NavBarLeftInput(page: Page, project: ProjectMetaData, rc: RouterCtl)

  private implicit val reusabilityNavBarLeftInput: Reusability[NavBarLeftInput] =
    Reusability.derive

  private def navBarLeft(input: NavBarLeftInput): MemberNavBar.LeftProps =
    Reusable.implicitly(input).map { i =>
      import i._

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

  private final case class NavBarRightInput(connectionStatus: ConnectionStatus, toggleConnectionStatus: Reusable[Callback])

  private implicit val reusabilityNavBarRightInput: Reusability[NavBarRightInput] =
    Reusability.derive

  private val connectedIcon =
    ConnectionStatus.memo {
      case ConnectionStatus.Connected =>
        Icon.Plug.withColour(Colour.Green).tag(Style.navBar.connected, ^.title := "connected")

      case ConnectionStatus.Disconnected =>
        Icon.Plug.withColour(Colour.Red).tag(Style.navBar.disconnected, ^.title := "disconnected")
    }

  private def navBarRight(input: NavBarRightInput): MemberNavBar.RightProps =
    Reusable.implicitly(input).map { i =>
      import i._

      val connectedMenuItem =
        Menu.Item(Menu.ItemType.Div(
          connectedIcon(connectionStatus)(^.onClick --> toggleConnectionStatus)))

      connectedMenuItem :: Nil
    }

  // -------------------------------------------------------------------------------------------------------------------

  private def render(p: Props): VdomElement = {
    val menuLeft  = navBarLeft(NavBarLeftInput(p.page, p.project, p.rc))
    val menuRight = navBarRight(NavBarRightInput(p.connectionStatus, p.setConnectionStatus(!p.connectionStatus)))
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
