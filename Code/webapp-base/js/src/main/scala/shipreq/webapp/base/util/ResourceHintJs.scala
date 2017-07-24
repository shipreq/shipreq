package shipreq.webapp.base.util

import org.scalajs.dom.document
import org.scalajs.dom.html
import scala.scalajs.js

object ResourceHintJs {

  type ResourceHint = shipreq.webapp.base.util.ResourceHint
  val  ResourceHint = shipreq.webapp.base.util.ResourceHint

  implicit class ResourceHintPreloadLikeExt(private val rh: ResourceHint.PreloadLike) extends AnyVal {
    def toLinkElement(): html.Link = {
      val link = document.createElement("link").asInstanceOf[html.Link]
      link.href = rh.href
      link.rel = rh.rel.value
      link.asInstanceOf[js.Dynamic].as = rh.as.value
      rh.`type`.foreach(link.`type` = _)
      rh.generic.crossorigin.foreach(link.asInstanceOf[js.Dynamic].crossOrigin = _)
      link
    }

    def install(): Unit =
      document.head.appendChild(toLinkElement())
  }

}
