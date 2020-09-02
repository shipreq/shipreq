package shipreq.webapp.server.snippet

import net.liftweb.http.DispatchSnippet
import net.liftweb.util.Helpers._
import scala.xml._
import shipreq.webapp.server.app.Global

object ScalaJsAssets extends DispatchSnippet {

  private val sjs = Global.config.server.scalaJsManifest

  override val dispatch: DispatchIt = {
    case "prefetchProject" => prefetchProject
    case "public"          => public
    case "home"            => home
    case "project"         => project
    case "webWorker"       => webWorker
  }

  private val prefetchProject: NodeSeq => NodeSeq =
    "*" #> <link data-lift="head" rel="prefetch" href={sjs.project} />

  private val public: NodeSeq => NodeSeq =
    "*" #> <script src={sjs.public} />

  private val home: NodeSeq => NodeSeq =
    "*" #> <script src={sjs.home} />

  private val project: NodeSeq => NodeSeq =
    "*" #> <script src={sjs.project} />

  private val webWorker: NodeSeq => NodeSeq =
    "*" #> <script src={sjs.webWorker} />
}
