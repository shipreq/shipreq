package shipreq.webapp.client.public.spa

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import shipreq.webapp.base.config.Urls.PublicSpaRoute.Login
import shipreq.webapp.base.config.{AssetManifest, Urls, WebappConfig}
import shipreq.webapp.base.data.Username
import shipreq.webapp.client.public.Styles.{layout => *}

object Layout {

  final case class Props(loggedInUser: Option[Username],
                         currentPage : Page,
                         am          : AssetManifest,
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

  private type Link = Urls.External \/ Page.Static
  private val linkSep = <.span(*.linkSep, "|")
  private val linkActive = <.span(*.linkActive)

  private def makeNav(p: Props, links: List[Link]): TagMod = {
    def renderExternal(e: Urls.External): VdomTag =
      <.a.toNewWindow(e.url.absoluteUrl)(e.title)

    def renderPage(page: Page.Static): VdomTag = {

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
      .map(_.fold(renderExternal, renderPage))
      .intersperse(linkSep)
      .toTagMod
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  private object Header {
    private val left    = <.div(*.headerSides)
    private val mid     = <.div(*.headerMid)
    private def right   = left
    private val links   = List[Link](
      -\/(Urls.External.about),
      \/-(Page.Login),
      \/-(Page.Register1),
    )

    private def logoImg(am: AssetManifest) =
      <.img(*.headerLogo, ^.src := am.shipreqLogoSvg, ^.alt := Page.Home.linkTitle)

    private def render(p: Props): VdomElement = {
      val logo =
        TagMod.unless(p.currentPage ==* Page.Home)(
          p.routerCtl.link(Page.Home)(logoImg(p.am)))

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
    private val links     = List[Link](
      \/-(Page.Privacy),
      \/-(Page.TermsOfService),
    )

    private def render(p: Props): VdomElement =
      footer(makeNav(p, links))

    val Component = ScalaComponent.builder[Props]
      .render_P(render)
      .build
  }

}