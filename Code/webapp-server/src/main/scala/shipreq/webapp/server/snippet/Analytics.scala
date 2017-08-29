package shipreq.webapp.server.snippet

import net.liftweb.util.Helpers._
import scala.xml._
import shipreq.webapp.base.AssetManifest
import shipreq.webapp.server.app.Global
import shipreq.webapp.server.lib.SingleOpStatelessSnippet

/**
  * Enables Google Analytics if the server was started with a tracking ID.
  */
object Analytics extends SingleOpStatelessSnippet {

  override val render: NodeSeq => NodeSeq = {
    val replacement = Global.config.googleAnalyticsTrackingId.fold(disable)(enable)
    "*" #> replacement
  }

  def disable: NodeSeq =
    Group(Nil)

  def enable(trackingId: String): NodeSeq = {
    val initErrorListener = "addEventListener('error',window.__e=function f(e){f.q=f.q||[];f.q.push(e)})"
    val initMain = s"ga2.i('$trackingId')" // window.ga2 is created at the bottom of analyticsJs
    Group(
      <script type="text/javascript" data-lift="head">{initErrorListener}</script> ::
      <script type="text/javascript" async="async" src={AssetManifest.analyticsJs} onload={initMain}></script> ::
      <script type="text/javascript" async="async" src="https://www.google-analytics.com/analytics.js"></script> ::
      Nil)
  }
}
