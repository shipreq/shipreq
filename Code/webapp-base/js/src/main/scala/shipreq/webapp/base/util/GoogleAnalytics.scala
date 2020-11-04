package shipreq.webapp.base.util

import japgolly.scalajs.react.{Callback, CallbackOption, CallbackTo}
import scala.scalajs.js
import shipreq.base.util.Url
import shipreq.webapp.base.config.AnalyticsConfig._

object GoogleAnalytics {

  private val GA: CallbackOption[js.Dynamic] =
    CallbackTo(js.Dynamic.global.ga.asInstanceOf[js.UndefOr[js.Dynamic]].toOption).asCBO

  def onRouteChange[P: UnivEq](prev: Option[P], current: P)(path: P => Url.Relative): Callback =
    Callback.when(prev.forall(_ !=* current))(
      sendPageview(path(current), "SPA Router"))

  private def sendPageview(path: Url.Relative, source: String): Callback =
    GA.map { ga =>

      ga("set", "page", path.relativeUrl)

      ga("send", "pageview", {
        val o = new js.Object()
        val d = o.asInstanceOf[js.Dictionary[String]]
        d.update(dimensions.HIT_SOURCE, source)
        o
      })

      ()
    }

}
