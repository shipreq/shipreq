package shipreq.webapp.client.ww

import org.scalajs.dom.webworkers.DedicatedWorkerGlobalScope
import scala.scalajs.js
import shipreq.webapp.base.AppConsts
import shipreq.webapp.client.ww.api.SVG

object GraphViz {
  type Fn = js.Function2[String, String, String]

  def JsUrl = AppConsts.assetPath_/ + "viz.js"

  lazy val instance: Fn = {
    DedicatedWorkerGlobalScope.self importScripts js.Array(JsUrl)
    js.Dynamic.global.Viz.asInstanceOf[Fn]
  }

  def apply(dot: DOT): SVG =
    SVG(instance(dot.content, "svg"))

  case class DOT(content: String) extends AnyVal {
    @inline def toSVG: SVG =
      GraphViz(this)
  }
}
