package shipreq.webapp.server.snippet

import net.liftweb.http.DispatchSnippet
import shipreq.webapp.base.WebappConfig.assetPath_/
import shipreq.webapp.server.lib.SnippetHelpers

/**
  * Includes assets (JS, CSS, images, etc) into a page.
  */
object Assets extends DispatchSnippet with SnippetHelpers {

  private def js(path: String) =
    <script type="text/javascript" src={assetPath_/ + path}></script>

  private def css(path: String) =
    <link data-lift="head" type="text/css" rel="stylesheet" href={assetPath_/ + path}/>

  private def png(path: String, alt: String) =
    staticHtml(<img src={assetPath_/ + path} alt={alt} />)

  override def dispatch = {
    case "favicon"      => Favicon
    case "public"       => Public
    case "homeSPA"      => HomeSPA
    case "projectSPA"   => ProjectSPA
    case "katex"        => Katex
    case "sir"          => Sir
    case "shipreq-huge" => ShipreqHuge
  }

  val Favicon = staticHtml(<link href={assetPath_/ + "favicon.ico"} type="image/x-icon" rel="icon"/>)

  val ShipreqHuge = png("shipreq-huge.png", "ShipReq")

  val PublicDepsJs = js("public-deps.js")

  val MemberDepsJs = js("member-deps.js")

  val Katex = staticHtml(Seq(js("katex.min.js"), css("katex.min.css")))

  val Public = staticHtml(Seq(PublicDepsJs, css("public.css")))

  val HomeSPA = staticHtml(Seq(MemberDepsJs, js("client-home.js")) ++ Katex(null))

  val ProjectSPA = staticHtml(Seq(MemberDepsJs, js("client-project.js")) ++ Katex(null))

  val Sir = staticHtml(Seq(css("sir.css")))
}
