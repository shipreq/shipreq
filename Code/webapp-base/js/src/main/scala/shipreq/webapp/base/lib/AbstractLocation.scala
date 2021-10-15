package shipreq.webapp.base.lib

import japgolly.scalajs.react._
import org.scalajs.dom.window
import shipreq.base.util.Url

trait AbstractLocation {
  def setHref        (url: Url.Absolute): Callback
  def setHrefRelative(url: Url.Relative): Callback
}

object AbstractLocation {

  object Real extends AbstractLocation {
    private[this] def set(href: String) = Callback {
      window.location.href = href
    }

    override def setHref        (url: Url.Absolute) = set(url.absoluteUrl)
    override def setHrefRelative(url: Url.Relative) = set(url.relativeUrl)
  }
}
