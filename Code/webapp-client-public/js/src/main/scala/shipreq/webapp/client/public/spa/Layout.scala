package shipreq.webapp.client.public.spa

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.univeq._
import shipreq.webapp.base.Urls.PublicSpaRoute.Login
import shipreq.webapp.base.user.Username
import shipreq.webapp.base.{AssetManifest, WebappConfig}
import shipreq.webapp.client.public.Styles.{layout => *}

object Layout {

  final case class Props(loggedInUser: Option[Username],
                         currentPage : Page,
                         routerCtl   : RouterCtl,
                         content     : VdomElement) {
    @inline def render: VdomElement = Component(this)
  }

  private def render(p: Props): VdomElement =
    <.div(*.cont,
      Header.Component(p),
      <.main(*.main, p.content),
      Footer.Component(p))

  val Component = ScalaComponent.builder[Props]
    .render_P(render)
    .build

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Shared

  private val linkSep = <.span(*.linkSep, "|")
  private val linkActive = <.span(*.linkActive)

  private def makeNav(p: Props, links: List[Page.Static]): TagMod = {
    def render(page: Page.Static): VdomTag = {

      val base: VdomTag =
        if (page ==* p.currentPage)
          linkActive
        else
          p.routerCtl.link(page)

      val title: TagMod =
        p.loggedInUser match {
          case Some(u) if page.route ==* Login => TagMod(*.loggedIn, u.with_@)
          case _                               => page.linkTitle
        }

      base(title)
    }

    links.iterator
      .map(render)
      .intersperse(linkSep)
      .toTagMod
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  private object Header {
    private val left    = <.div(*.headerSides)
    private val mid     = <.div(*.headerMid)
    private def right   = left
    private val logoImg = <.img(*.headerLogo, ^.src := AssetManifest.shipreqLogoSvg, ^.alt := Page.Home.linkTitle)
    private val links   = List[Page.Static](Page.Login, Page.Register1)

    private def render(p: Props): VdomElement = {
      val logo =
        TagMod.unless(p.currentPage ==* Page.Home)(
          p.routerCtl.link(Page.Home)(logoImg))

      <.header(*.header,
        left(logo),
        mid(makeNav(p, links)),
        right)
    }

    val Component = ScalaComponent.builder[Props]
      .render_P(render)
      .build
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  private object Footer {
    private def copyright = <.div(*.copyright, WebappConfig.copyrightNotice)
    private val footer    = <.footer(*.footer, copyright)
    private val links     = List[Page.Static](Page.TermsOfService, Page.Privacy)

    private def render(p: Props): VdomElement =
      footer(makeNav(p, links))

    val Component = ScalaComponent.builder[Props]
      .render_P(render)
      .build
  }

}