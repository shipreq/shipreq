package shipreq.webapp.client.base.ui

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import shipreq.webapp.base.data.Username
import shipreq.webapp.base.{URLs, WebappConfig}
import shipreq.webapp.client.base.ClientConfig
import shipreq.webapp.client.base.ui.semantic.{Breadcrumb, Dropdown, Icon, Menu, SemExtAny}

/** At top of member (logged-in) screens:
  *
  * +------------------------------------------------------------------+
  * | [logo]  Projects > Fart > Reqs > UC-1                  @username |
  * +------------------------------------------------------------------+
  */
object MemberNavBar {

  case class Props(username: Username,
                   left    : Breadcrumb.Items,
                   right   : Dropdown.Items) {
    val leftWithDividers = left.iterator.intersperse(Divider).toList
    @inline def render = Component(this)
  }

  private val menuStyle =
    Menu.Style(Menu.Attr.Borderless + Menu.Attr.Fixed + Menu.Attr.Inverted)

  private val itemLogo =
    Menu.Item.Div(
      <.img(
        ^.src := URLs.SvgShipreqCircleDark,
        ^.alt := WebappConfig.appName))

  private val breadcrumbStyle =
    Breadcrumb.Style()

  private val dropdownLogout =
    Dropdown.Item.Link(
      <.a(^.href := URLs.PageLogout, "Logout"))

  // implicit val reusabilityProps: Reusability[Props] =
  //   Reusability.caseClass

  final class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props): ReactElement = {
      val leftBreadcrumb =
        Menu.Item.Div(
          Breadcrumb.Props(breadcrumbStyle, p.leftWithDividers).render)

      val rightDropdown =
        Menu.Item.DropdownSimple(
          p.username.with_@,
          p.right :+ dropdownLogout)

      val menu = Menu.Props(
        menuStyle,
        itemLogo :: leftBreadcrumb :: Nil,
        rightDropdown :: Nil)

      <.nav(
        BaseStyles.navBarContainer,
        menu.render)
    }
  }

  val Component = ReactComponentB[Props]("NavBar")
    .renderBackend[Backend]
    // .configure(Reusability.shouldComponentUpdate) TODO
    .build


  // ===================================================================================================================
  //  Common items

  val MemberHome =
    Breadcrumb.Item.Link(
      <.a(
        ^.href := URLs.PageMemberHome,
        ClientConfig.BreadcrumbNameMemberHome))

  val Divider =
    Breadcrumb.Item.DividerIcon(
      Icon.AngleRight,
      BaseStyles.breadcrumbDivider)
}
