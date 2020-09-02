package shipreq.webapp.server.snippet

import net.liftweb.http.DispatchSnippet
import scala.xml._
import shipreq.webapp.server.app.Global
import shipreq.webapp.server.app.ServerConfig.Statcounter

/**
  * Enables Google Analytics if the server was started with a tracking ID.
  */
object Analytics extends DispatchSnippet with SnippetHelpers {

  override val dispatch: DispatchIt = {

    val logic: Logic[NodeSeq => NodeSeq] =
      foldLogic(all.flatMap(_.toList), List.empty[Node])(_ ++ _)
        .map(_.toVector)
        .map(Group.apply)
        .map(g => _ => g)

    {
      case "multiRoute"  => logic.multiRoute
      case "singleRoute" => logic.singleRoute
    }
  }

  private final case class Logic[+A](singleRoute: A, multiRoute: A) {
    def map[B](f: A => B): Logic[B] =
      Logic(
        singleRoute = f(singleRoute),
        multiRoute = f(multiRoute),
      )
  }

  private def foldLogic[A, B](as: List[Logic[A]], b: B)(f: (B, A) => B): Logic[B] =
    as.foldLeft(Logic(b, b))((q, l) => Logic(
      singleRoute = f(q.singleRoute, l.singleRoute),
      multiRoute = f(q.multiRoute, l.multiRoute),
    ))

  // ===================================================================================================================

  private def all: List[Option[Logic[Group]]] =
    List(
      Global.config.server.googleAnalyticsTrackingId.map(googleAnalytics),
      Global.config.statcounter.map(statcounter),
    )

  private def googleAnalytics(trackingId: String): Logic[Group] = {
    // www.google-analytics.com/analytics.js
    val url = Global.analyticsProxy.masked("*(d3d3Lmdvb2dsZS1hbmFseXRpY3MuY29t)*/*(YW5hbHl0aWNzLmpz)*")

    // so that analytics from Scala.JS aren't lost
    val initGA = "window.ga=function(){ga.q.push(arguments)};ga.q=[]"

    // required by analyticsJs, see https://philipwalton.com/articles/the-google-analytics-setup-i-use-on-every-site-i-build/
    val initErr = "addEventListener('error',window.__e=function f(e){f.q=f.q||[];f.q.push(e)})"

    // window.ga2 is created at the bottom of analyticsJs
    val initAs = s"ga2.i('$trackingId',1)" // sets sendPageviewNow to true
    val initAm = s"ga2.i('$trackingId')"   // SPA Router sends initial pageview

    val scriptInit = <script type="text/javascript" data-lift="head">{initErr};{initGA}</script>
    val scriptGA   = <script type="text/javascript" async="async" src={url.absoluteUrl}></script>
    val scriptAs   = <script type="text/javascript" async="async" src={assetManifest.analyticsJs} onload={initAs}></script>
    val scriptAm   = <script type="text/javascript" async="async" src={assetManifest.analyticsJs} onload={initAm}></script>

    Logic(
      singleRoute = Group(scriptInit :: scriptAs :: scriptGA :: Nil),
      multiRoute  = Group(scriptInit :: scriptAm :: scriptGA :: Nil),
    )
  }

  private def statcounter(cfg: Statcounter): Logic[Group] = {
    // www.statcounter.com/counter/counter.js
    val url = Global.analyticsProxy.masked("*(d3d3LnN0YXRjb3VudGVyLmNvbQ)*/*(Y291bnRlcg)*/*(Y291bnRlci5qcw)*")

    val vars = Map[String, String](
      "sc_project"     -> cfg.project.toString,
      "sc_security"    -> s"'${cfg.security}'",
      "sc_invisible"   -> "1",
      "sc_https"       -> "1",
      "sc_remove_link" -> "1",
    )

    val setVarsJs = vars.iterator.map(x => x._1 + "=" + x._2).mkString("var ", ",", "")

    val script1 = <script type="text/javascript" data-lift="head">{setVarsJs}</script>
    val script2 = <script type="text/javascript" async="async" src={url.absoluteUrl}></script>

    val all = Group(script1 :: script2 :: Nil)

    Logic(all, all)
  }

}
