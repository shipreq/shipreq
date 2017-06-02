package shipreq.webapp.client.base.ui

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.data.Username
import shipreq.webapp.base.{AssetManifest, URLs, WebappConfig}
import shipreq.webapp.client.base.ClientConfig
import shipreq.webapp.client.base.lib.DataReusability._
import shipreq.webapp.client.base.ui.semantic.{Breadcrumb, Dropdown, Icon, Menu, SemExtAny}

/** At top of member (logged-in) screens:
  *
  * +------------------------------------------------------------------+
  * | [logo]  Projects > Fart > Reqs > UC-1                  @username |
  * +------------------------------------------------------------------+
  */
object MemberNavBar {

  type LeftProps = Reusable[Breadcrumb.Items]
  type RightProps = Reusable[Dropdown.Items]

  final case class Props(username: Username,
                         left    : LeftProps,
                         right   : RightProps = onlyUsernameOnTheRight) {
    lazy val leftWithDividers = left.iterator.intersperse(Divider).toList
    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.caseClass

  val onlyUsernameOnTheRight: RightProps =
    Reusable.byRef(Nil)

  private val menuStyle =
    Menu.Style(Menu.Attr.Borderless + Menu.Attr.Fixed + Menu.Attr.Inverted)

  private val itemLogo =
    Menu.Item.Div(
      <.img(
        ^.src := AssetManifest.shipreqCircleDarkSvg,
        ^.alt := WebappConfig.appName))

  private val breadcrumbStyle =
    Breadcrumb.Style()

  private val dropdownLogout =
    Dropdown.Item.Link(
      <.a(^.href := URLs.logout, "Logout"))

  final class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props): VdomElement = {
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

  val Component = ScalaComponent.builder[Props]("NavBar")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build

  // ===================================================================================================================
  //  Common items

  val MemberHome =
    Breadcrumb.Item.Link(
      <.a(
        ^.href := URLs.memberHome,
        ClientConfig.BreadcrumbNameMemberHome))

  val Divider =
    Breadcrumb.Item.DividerIcon(
      Icon.AngleRight,
      BaseStyles.breadcrumbDivider)
}
