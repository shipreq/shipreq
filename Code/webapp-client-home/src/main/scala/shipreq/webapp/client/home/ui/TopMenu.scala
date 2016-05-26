package shipreq.webapp.client.home.ui

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.prefix_<^._
import shipreq.webapp.base.data.Username
import shipreq.webapp.base.{URLs, WebappConfig}
import shipreq.webapp.client.base.ui.semantic.{Breadcrumb, Dropdown, Menu, SemExtAny}

object TopMenu {
  type Props = Username

  val menuStyle =
    Menu.Style(Menu.Attr.Borderless + Menu.Attr.Fixed + Menu.Attr.Inverted)

  val itemLogo =
    Menu.Item.Div(
      <.img(
        ^.src := URLs.SvgShipreqCircleDark,
        ^.alt := WebappConfig.appName))

  val breadcrumbStyle =
    Breadcrumb.Style()

  val breadcrumbItems =
    Breadcrumb.Item.Div("Projects") :: Nil

  val itemBreadcrumb =
    Menu.Item.Div(
      Breadcrumb.Props(breadcrumbStyle, breadcrumbItems).render)

  val dropdownLogout =
    Dropdown.Item.Link(
      <.a(^.href := URLs.PageLogout, "Logout"))

  private def render(p: Props): ReactElement = {
    val itemDropdown =
      Menu.Item.DropdownSimple(
        p.with_@,
        dropdownLogout :: Nil)

    val menu = Menu.Props(
      menuStyle,
      itemLogo :: itemBreadcrumb :: Nil,
      itemDropdown :: Nil)

    menu.render
  }

  val Component = FunctionalComponent(render)
}
