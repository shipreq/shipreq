package shipreq.webapp.server.snippet

import net.liftweb.http.DispatchSnippet
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq
import shipreq.webapp.base.WebappConfig
import shipreq.webapp.server.ServerConfig
import shipreq.webapp.server.lib.Misc

object Prop extends DispatchSnippet {
  override def dispatch = { case s => renderMemo(s) }

  val renderMemo = Misc.newMemo[String, NodeSeq => NodeSeq]()(render)

  def render(n: String): NodeSeq => NodeSeq =
    n match {

      case "appName" =>
        "*" #> WebappConfig.appName

      case "supportEmailLink" =>
        "a [href]" #> ("mailto:" + ServerConfig.SupportEmailAddress)
    }
}