package shipreq.webapp.client.public.spa

import japgolly.microlibs.stdlib_ext.StdlibExt._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra._
import japgolly.univeq._
import shipreq.webapp.base.{AssetManifest, WebappConfig}
import shipreq.webapp.base.lib.BaseReusability._
import shipreq.webapp.client.public.Styles.{layout => *}

object Layout {

  final case class Props(currentPage: Page, routerCtl: RouterCtl, content: VdomElement) {
    @inline def render: VdomElement = Component(this)
  }

  private def render(p: Props): VdomElement =
    <.div(*.cont,
      Header.Component(p),
      <.main(*.main, p.content),
      Footer.Component(p))

  val Component = ScalaComponent.builder[Props]("Layout")
    .render_P(render)
    .build

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  // Shared

  private val linkSep = <.span(*.linkSep, "|")
  private val linkActive = <.span(*.linkActive)

  private def makeNav(p: Props, links: List[Page.Static]): TagMod = {
    def render(page: Page.Static): VdomTag = {
      val base = if (p.currentPage ==* page) linkActive else p.routerCtl.link(page)
      base(page.linkTitle)
    }

    links.iterator
      .map(render)
      .intersperse(linkSep)
      .toTagMod
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  private object Header {
    val left    = <.div(*.headerSides)
    val mid     = <.div(*.headerMid)
    def right   = left
    val logoImg = <.img(*.headerLogo, ^.src := AssetManifest.shipreqLogoSvg, ^.alt := Page.Home.linkTitle)
    val links   = List[Page.Static](Page.Login, Page.Register1)

    def render(p: Props): VdomElement = {
      val logo =
        TagMod.unless(p.currentPage ==* Page.Home)(
          p.routerCtl.link(Page.Home)(logoImg))

      <.header(*.header,
        left(logo),
        mid(makeNav(p, links)),
        right)
    }

    val Component = ScalaComponent.builder[Props]("Layout.Header")
      .render_P(render)
      .build
  }

  // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  private object Footer {
    def copyright = <.span(*.footerTxt, WebappConfig.copyrightNotice)
    val footer    = <.footer(*.footer, copyright, linkSep)
    val links     = List[Page.Static](Page.TermsOfService, Page.Privacy)

    def render(p: Props): VdomElement =
      footer(makeNav(p, links))

    val Component = ScalaComponent.builder[Props]("Layout.Footer")
      .render_P(render)
      .build
  }

}