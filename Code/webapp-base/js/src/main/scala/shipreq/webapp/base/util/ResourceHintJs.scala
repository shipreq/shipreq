package shipreq.webapp.base.util

import org.scalajs.dom.{document, html}
import scala.scalajs.js

object ResourceHintJs {

  type ResourceHint = shipreq.webapp.base.util.ResourceHint
  val  ResourceHint = shipreq.webapp.base.util.ResourceHint

  implicit class ResourceHintPreloadLikeExt(private val rh: ResourceHint) extends AnyVal {
    def toLinkElement(): html.Link = {
      val link = document.createElement("link").asInstanceOf[html.Link]
      val g = rh.generic
      link.rel = g.rel
      link.href = g.href
      g.`type`.foreach(link.`type` = _)
      g.as.foreach(link.asInstanceOf[js.Dynamic].as = _)
      g.crossorigin.foreach(link.asInstanceOf[js.Dynamic].crossOrigin = _)
      link
    }

    def install(): Unit =
      document.head.appendChild(toLinkElement())
  }

}
