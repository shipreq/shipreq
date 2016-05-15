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
    case "favicon"     => favicon
    case "anon"        => anon
    case "project"     => project
    case "katex"       => katex
    case "sir"         => sir
    case "shipreq-png" => shipreqPng
  }

  val favicon = staticHtml(
      <link href={assetPath_/ + "favicon.ico"} type="image/x-icon" rel="icon"/>)

  val shipreqPng =
    png("shipreq.png", "ShipReq")

  val katex = staticHtml(Seq(
    js ("katex.min.js"),
    css("katex.min.css")))

  val anon = staticHtml(Seq(
    js ("anon.js"),
    css("app.css")))

  val project = staticHtml(Seq(
    css("app.css"),
    js("project.js"),
    js("client-project.js")) ++
    katex(null))

  val sir = staticHtml(Seq(
    css("sir.css")))
}
