package shipreq.webapp.client.ww

import org.scalajs.dom.console
import org.scalajs.dom.webworkers.DedicatedWorkerGlobalScope
import scala.scalajs.js
import shipreq.webapp.base.AssetManifest
import shipreq.webapp.client.ww.api.SVG

object GraphViz {
  type Fn = js.Function2[String, String, String]

  lazy val instance: Fn = {
    DedicatedWorkerGlobalScope.self importScripts js.Array(AssetManifest.vizJs)
    js.Dynamic.global.Viz.asInstanceOf[Fn]
  }

  private val titlesAndComments = "(?:<title>[^<>]*?</title>|<!--[^\u0000]*?-->)".r

  def apply(dot: DOT): SVG =
    try {
      var svg = instance(dot.content, "svg")
      svg = titlesAndComments.replaceAllIn(svg, "")
      SVG(svg)
    } catch {
      case t: Throwable =>
        console.error("GraphViz crash: ", dot.content)
        throw t
    }

  case class DOT(content: String) extends AnyVal {
    @inline def toSVG: SVG =
      GraphViz(this)
  }
}
