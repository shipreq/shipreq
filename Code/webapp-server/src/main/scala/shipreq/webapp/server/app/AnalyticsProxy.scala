package shipreq.webapp.server.app

import japgolly.clearconfig._
import shipreq.base.util.Url

/** Nearly all ad-blockers block popular analytics products too like Google Analytics and statcounter.
 *
 * To avoid our analytics requests being intercepted by ad-blockers, we re-route analysis traffic through our own
 * proxy. The source is in /Docker/analytics_proxy.
 *
 * @see https://github.com/ZitRos/save-analytics-from-content-blockers
 * @see https://www.freecodecamp.org/news/save-your-analytics-from-content-blockers-7ee08c6ec7ee/
 */
final class AnalyticsProxy(proxyUrl: Url.Absolute) {

  // Eg. https://ap.shipreq.com/
  //  or https://shipreq.com/ap/
  private[this] val prefix =
    proxyUrl.absoluteUrl
      .replaceFirst("/*$", "/") // ensure ends in a slash

  /** Uses a masked (i.e. obfuscated) URL.
    *
    * See /Docker/analytics_proxy/README.md for instructions on how to generate these values.
    */
  def masked(path: String): Url.Absolute =
    Url.Absolute(prefix + path.dropWhile(_ == '/'))

  @deprecated("Use masked", "")
  def reRoute(url: Url.Absolute): Url.Absolute =
    Url.Absolute(url.absoluteUrl.replaceFirst("^.+?://", prefix))
}

object AnalyticsProxy {

  final case class Config(url: String) {
    def build: AnalyticsProxy =
      new AnalyticsProxy(Url.Absolute(url))
  }

  def config: ConfigDef[Config] =
    ConfigDef.need[String]("url")
      .map(Config.apply)
      .withPrefix("analytics_proxy.")

}