package shipreq.webapp.server.snippet

import net.liftweb.http.DispatchSnippet
import scala.xml._
import shipreq.base.util.Url
import shipreq.webapp.base.AssetManifest
import shipreq.webapp.server.app.Global

/**
  * Enables Google Analytics if the server was started with a tracking ID.
  */
object Analytics extends DispatchSnippet {

  override val dispatch: DispatchIt =
    Global.config.server.googleAnalyticsTrackingId match {

      case None =>
        val remove: NodeSeq => NodeSeq = Function const Group(Nil)

        { case _ => remove }

      case Some(trackingId) =>

        val gaUrl = Global.analyticsProxy.reRoute(Url.Absolute("https://www.google-analytics.com/analytics.js"))

        // so that analytics from Scala.JS aren't lost
        val initGA = "window.ga=function(){ga.q.push(arguments)};ga.q=[]"

        // required by analyticsJs, see https://philipwalton.com/articles/the-google-analytics-setup-i-use-on-every-site-i-build/
        val initErr = "addEventListener('error',window.__e=function f(e){f.q=f.q||[];f.q.push(e)})"

        // window.ga2 is created at the bottom of analyticsJs
        val initAs = s"ga2.i('$trackingId',1)" // sets sendPageviewNow to true
        val initAm = s"ga2.i('$trackingId')"   // SPA Router sends initial pageview

        val scriptInit = <script type="text/javascript" data-lift="head">{initErr};{initGA}</script>
        val scriptGA   = <script type="text/javascript" async="async" src={gaUrl.absoluteUrl}></script>
        val scriptAs   = <script type="text/javascript" async="async" src={AssetManifest.analyticsJs} onload={initAs}></script>
        val scriptAm   = <script type="text/javascript" async="async" src={AssetManifest.analyticsJs} onload={initAm}></script>

        val singleRoute = Group(scriptInit :: scriptAs :: scriptGA :: Nil)
        val multiRoute  = Group(scriptInit :: scriptAm :: scriptGA :: Nil)

        {
          case "multiRoute"  => _ => multiRoute
          case "singleRoute" => _ => singleRoute
        }
    }
}
