package shipreq.webapp.base.ui

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.user.Username
import shipreq.webapp.base.{AssetManifest, Urls, WebappConfig}
import shipreq.webapp.base.ClientConfig
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.ui.semantic.{Breadcrumb, Dropdown, Icon, Menu, SemExtAny}
import shipreq.webapp.base.ui.semantic.Dropdown.JsOptionsOps
import BaseStyles.{layout => *}

/** At top of member (logged-in) screens:
  *
  * +------------------------------------------------------------------+
  * | [logo]  Projects > Fart > Reqs > UC-1                  @username |
  * +------------------------------------------------------------------+
  */
object MemberNavBar {

  type LeftProps = Reusable[Breadcrumb.Items]
  type RightProps = Reusable[Menu.Items]

  val noRightProps: RightProps = Reusable.byRef(Nil)

  final case class Props(username     : Username,
                         feedbackModal: Option[FeedbackModal],
                         left         : LeftProps,
                         right        : RightProps = noRightProps) {
    lazy val leftWithDividers = left.iterator.intersperse(Divider).toList
    @inline def render: VdomElement = Component(this)
  }

  implicit val reusabilityProps: Reusability[Props] =
    Reusability.derive

  private val menuStyle =
    Menu.Style(
      attr = Menu.Attr.Borderless + Menu.Attr.Inverted,
      tagMod = *.navMenu)

  private val dropdownOptions =
    Dropdown.JsOptions.readOnly

  private val itemLogo =
    Menu.ItemType.Link(
      <.a(
        ^.href := Urls.publicHome.relativeUrl,
        <.img(
          ^.src := AssetManifest.shipreqCircleDarkSvg,
          ^.alt := WebappConfig.appName))
    ).toItem

  private val breadcrumbStyle =
    Breadcrumb.Style()

  private val preventDefault: ReactEvent => Callback =
    _.preventDefaultCB

  private val dropdownLogout =
    Dropdown.Item.Link(
      <.a(^.href := Urls.logout.relativeUrl, ^.onClick ==> preventDefault, "Logout"))

  final class Backend($: BackendScope[Props, Unit]) {

    def render(p: Props): VdomElement = {
      val leftBreadcrumb =
        Menu.ItemType.Div(
          Breadcrumb.Props(breadcrumbStyle, p.leftWithDividers).render
        ).toItem

      val dropdownSendFeedback = {
        val root = <.a("Send feedback")
        p.feedbackModal match {
          case Some(m) => Dropdown.Item.Link(root(^.onClick ==> preventDefault.andThen(_ >> m.run.toCallback)))
          case None    => Dropdown.Item.Link(root(^.disabled := true), Dropdown.ItemState.Disabled)
        }
      }

      val rightDropdown =
        Menu.DropdownType.Simple(
          p.username.with_@,
          dropdownSendFeedback :: dropdownLogout :: Nil
        ).toItem

      val leftMenuItems =
        itemLogo :: leftBreadcrumb :: Nil

      val rightMenuItems =
        p.right :+ rightDropdown

      val menu = Menu.Props(
        menuStyle,
        leftMenuItems,
        rightMenuItems,
        dropdownOptions)

      <.nav(menu.render)
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
        ^.href := Urls.memberHome.relativeUrl,
        ClientConfig.BreadcrumbNameMemberHome))

  val Divider =
    Breadcrumb.Item.DividerIcon(
      Icon.AngleRight,
      *.navBreadcrumbDivider)
}
