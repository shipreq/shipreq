package shipreq.webapp.base.test

import japgolly.scalajs.react._
import shipreq.base.util.Url
import shipreq.webapp.base.lib.AbstractLocation

final class TestLocation(initial: Url.Absolute) extends AbstractLocation {

  var href = initial

  override def setHref(url: Url.Absolute) = Callback {
    href = url
  }

  override def setHrefRelative(url: Url.Relative) = Callback {
    href = href / url
  }
}

object TestLocation {
  def apply(): TestLocation =
    new TestLocation(Url.Absolute("http://localhost"))
}
