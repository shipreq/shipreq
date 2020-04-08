package shipreq.webapp.client.project.app.pages.root

import japgolly.microlibs.stdlib_ext.MutableArray
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.ScalaCssReact._
import shipreq.webapp.base.data.{Project, ProjectMetaData}
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.text.PlainText
import shipreq.webapp.base.ui._
import shipreq.webapp.base.ui.semantic.Menu.DropdownType
import shipreq.webapp.base.ui.semantic.{Breadcrumb, Colour, Dropdown, Icon, Menu, Size}
import shipreq.webapp.base.user.Username
import shipreq.webapp.client.project.app.Style
import shipreq.webapp.client.project.widgets.{FilterHelp, RichTextEditorHelp}

object Layout {
  import Routes.{Page, RouterCtl}

  final case class Props(username           : Username,
                         project            : ProjectMetaData,
                         unsavedChanges     : UnsavedChangeData,
                         connectionStatus   : ConnectionStatus,
                         setConnectionStatus: ConnectionStatus => Reusable[Callback],
                         reauthModal        : ReauthenticationModal,
                         feedbackModal      : FeedbackModal,
                         toast              : Toast.Props,
                         rc                 : RouterCtl,
                         page               : Page,
                         content            : VdomElement) {
    @inline def render = Component(this)
  }

  // ===================================================================================================================

  private final case class NavBarLeftInput(page: Page, project: ProjectMetaData, rc: RouterCtl)

  private val navBarLeft: NavBarLeftInput => MemberNavBar.LeftProps =
    Reusable.fnOutput.explicitly(Reusability.derive[NavBarLeftInput]) { i =>
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

  // ===================================================================================================================

  private final case class NavBarRightInput(unsaved               : UnsavedChangeData,
                                            connectionStatus      : ConnectionStatus,
                                            toggleConnectionStatus: Reusable[Callback])

  final case class UnsavedChangeData(changes: UnsavedChanges,
                                     locs   : List[(String, Page)],
                                     rc     : RouterCtl) {

    private[Layout] val menuItem = {

      val dropdownItems: List[Dropdown.Item] =
        locs.map { case (title, page) =>
          // NOTE: Don't use rc.setOnClick here. It will only work the first time its clicked because for whatever
          // reason (cough cough Semantic UI), preventDefault is set after the first click and rc.setOnClick will
          // only execute if preventDefault isn't set.
          Dropdown.Item.Div(TagMod(
            title,
            ^.onClick --> rc.set(page)))
        }

      val header = TagMod(
        Style.navBar.unsavedChangesItem,
        ^.title := changes.descShort,
        <.span(Style.navBar.unsavedChangesText, changes.count),
        unsavedChangesIcon)

      Menu.Item(Menu.ItemType.Dropdown(DropdownType.Simple, header, dropdownItems))
    }
  }

  object UnsavedChangeData {

    def derive(changes: UnsavedChanges,
               p      : Project,
               rc     : RouterCtl): UnsavedChangeData = {

      val locs =
        MutableArray(changes.locations).map {

          case UnsavedChanges.Location.Req(id) =>
            val eid = p.content.reqs.need(id).pubid.external(p)
            PlainText.pubid(eid) -> Page.ReqDetail(eid)

          case UnsavedChanges.Location.ManualIssues     => "Manual Issue(s)"            -> Page.Issues
          case UnsavedChanges.Location.ProjectName      => "Project Name"               -> Page.Index
          case UnsavedChanges.Location.FieldConfig      => "Field Editor"               -> Page.CfgFields
          case UnsavedChanges.Location.IssueConfig      => "Issue Type Editor"          -> Page.CfgIssues
          case UnsavedChanges.Location.ReqTypeConfig    => "Req Type Editor"            -> Page.CfgReqTypes
          case UnsavedChanges.Location.TagConfig        => "Tag Editor"                 -> Page.CfgTags
          case UnsavedChanges.Location.ReqCodeGroup(id) => PlainText.reqCodeById(id, p) -> Page.ReqTable
        }.sortBy(_._1).iterator.toList

      apply(changes, locs, rc)
    }

    implicit def reusability: Reusability[UnsavedChangeData] =
      Reusability.byRef || Reusability.derive
  }

  private val connectedIcon =
    ConnectionStatus.memo {
      case ConnectionStatus.Connected =>
        Icon.Plug.withColour(Colour.Green).tag(Style.navBar.connected, ^.title := "connected")

      case ConnectionStatus.Disconnected =>
        Icon.Plug.withColour(Colour.Red).tag(Style.navBar.disconnected, ^.title := "disconnected")
    }

  private val unsavedChangesIcon =
    Icon.Edit.withSize(Size.Large).tag(Style.navBar.unsavedChangesIcon)

  private val navBarRight: NavBarRightInput => MemberNavBar.RightProps =
    Reusable.fnOutput.explicitly(Reusability.derive[NavBarRightInput]) { i =>
      import i._

      var menuItems = List.empty[Menu.Item]

      // Connection status
      menuItems ::=
        Menu.Item(Menu.ItemType.Div(
          connectedIcon(connectionStatus)(^.onClick --> toggleConnectionStatus)))

      // Unsaved changes
      if (unsaved.changes.nonEmpty)
        menuItems ::= unsaved.menuItem

      menuItems
    }

  // ===================================================================================================================

  private def render(p: Props): VdomElement = {

    val menuLeft = navBarLeft(NavBarLeftInput(
      p.page,
      p.project,
      p.rc))

    val menuRight = navBarRight(NavBarRightInput(
      p.unsavedChanges,
      p.connectionStatus,
      p.setConnectionStatus(!p.connectionStatus)))

    val navBar = MemberNavBar.Props(
      p.username,
      Some(p.feedbackModal),
      menuLeft,
      menuRight)

    MemberLayout.Props(
      navBar,
      <.div(
        _,
        Style.layout,
        FilterHelp.modal.render,
        Toast.Component(p.toast),
        p.feedbackModal.render,
        p.reauthModal.render,
        RichTextEditorHelp.allRendered,
        p.content))
      .render
  }

  val Component = ScalaFnComponent(render)
}
