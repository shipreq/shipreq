package shipreq.webapp.server.snippet

import net.liftweb.http.DispatchSnippet
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq
import shipreq.base.util.Memo
import shipreq.webapp.base.WebappConfig
import shipreq.webapp.server.app.DI

object Config extends DispatchSnippet {
  override def dispatch = { case s => renderMemo(s) }

  val renderMemo = Memo[String, NodeSeq => NodeSeq] {

    case "appName" =>
      "*" #> WebappConfig.appName

    case "supportEmailLink" =>
      "a [href]" #> ("mailto:" + DI.serverConfig.supportEmailAddress)
  }
}