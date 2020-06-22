package shipreq.webapp.base.ui

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.lib.DataReusability._
import shipreq.webapp.base.ui.BaseStyles.{layout => *}
import shipreq.webapp.base.ui.semantic.{Breadcrumb, Dropdown, Icon, Menu, SemExtAny}
import shipreq.webapp.base.user.Username
import shipreq.webapp.base.{AssetManifest, ClientConfig, Urls, WebappConfig}

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

  private val sendFeedbackTitle = "Send feedback"
  private val sendFeedbackRoot  = <.div(sendFeedbackTitle)

  private val dropdownOptions = {
    val preventSelection: Set[String] = Set(sendFeedbackTitle)
    new Dropdown.JsOptions {
      override val action = Dropdown.JsOptions.Action.custom { args =>
        val text = args.element.innerText.trim
        if (!preventSelection.contains(text))
          args.select()
        args.hideAndClear()
      }
      on = Dropdown.JsOptions.On.Hover
    }
  }

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

  private val dropdownLogout =
    Dropdown.Item.Link(
      <.a(^.href := Urls.logout.relativeUrl, "Logout"))

  private def render(p: Props): VdomElement = {
    val leftBreadcrumb =
      Menu.ItemType.Div(
        Breadcrumb.Props(breadcrumbStyle, p.leftWithDividers).render
      ).toItem

    val dropdownSendFeedback =
      p.feedbackModal match {
        case Some(m) => Dropdown.Item.Div(sendFeedbackRoot(^.onClick --> m.run.toCallback))
        case None    => Dropdown.Item.Div(sendFeedbackRoot(^.disabled := true), Dropdown.ItemState.Disabled)
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

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
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
