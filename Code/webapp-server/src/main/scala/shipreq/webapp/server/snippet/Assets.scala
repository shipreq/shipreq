package shipreq.webapp.server.snippet

import net.liftweb.http.DispatchSnippet
import scala.xml.NodeSeq
import japgolly.microlibs.nonempty.NonEmptyVector
import shipreq.webapp.base.WebappConfig.assetPath_/
import Asset._

sealed trait Asset {
  val tag: xml.Elem

  val transformer: NodeSeq => NodeSeq =
    _ => tag
}

object Asset {
  /** Loadable via loadjs */
  sealed trait LoadJsOk extends Asset {
    val path: String
  }

  final class JS(val path: String) extends LoadJsOk {
    override val tag = <script type="text/javascript" src={path}></script>
  }

  final class CSS(val path: String) extends LoadJsOk {
    override val tag = <link data-lift="head" type="text/css" rel="stylesheet" href={path}/>
  }

  final class Image(val path: String, val alt: String) extends Asset {
    override val tag = <img src={path} alt={alt} />
  }

  final class Favicon(val path: String) extends Asset {
    override val tag = <link href={path} type="image/x-icon" rel="icon"/>
  }

  def JS   (path: String)             : JS    = new JS   (assetPath_/ + path)
  def CSS  (path: String)             : CSS   = new CSS  (assetPath_/ + path)
  def Image(path: String, alt: String): Image = new Image(assetPath_/ + path, alt)

  final class Bundle[+T <: Asset](val assets: NonEmptyVector[T]) {
    val nodeSeq    : NodeSeq            = assets.whole.map(_.tag)
    val transformer: NodeSeq => NodeSeq = _ => nodeSeq

    def ++[TT >: T <: Asset](b: Bundle[TT]): Bundle[TT] =
      new Bundle(assets ++ b.assets)
  }

  implicit def autoToBundle[T <: Asset](a: T): Bundle[T] =
    new Bundle(NonEmptyVector one a)

  /** Pages with huge JS appear to load slowly due to JS fetching, parsing and compiling.
    * Performance perception can be greatly improved by deferring as much JS as possible until after the page has been
    * rendered.
    *
    * @param init Assets to load immediately and synchronously on page load.
    * @param next JS to load asynchronously after the page has loaded.
    */
  case class InitAndNext[I <: Asset](init: Bundle[I], next: Bundle[LoadJsOk]) {
    val all = init ++ next

    val nextPathsArray: String =
      next.assets.iterator.map("'" + _.path + "'").mkString("[", ",", "]")

    def addNext(js: Bundle[LoadJsOk]): InitAndNext[I] =
      InitAndNext(init, next ++ js)
  }
}

// =====================================================================================================================

/**
  * Includes assets (JS, CSS, images, etc) into a page.
  */
object Assets extends DispatchSnippet {

  override def dispatch = {
    case "favicon"      => Favicon        .transformer
    case "public"       => Public         .transformer
    case "homeSpa"      => HomeSpa        .transformer
    case "projectSpa"   => ProjectSpa.init.transformer
    case "sir"          => Sir            .transformer
    case "shipreq-huge" => ShipreqHuge    .transformer
  }

  // -----------
  // - Generic -
  // -----------

  val Favicon = new Favicon(assetPath_/ + "favicon.ico")

  val KatexJS  = JS ("katex.min.js")
  val KatexCSS = CSS("katex.min.css")
  val Katex    = KatexJS ++ KatexCSS

  val ShipreqHuge = Image("shipreq-huge.png", "ShipReq")

  val MemberDeps = InitAndNext(
    JS("member-deps-init.js") ++ CSS("member.css"),
    JS("member-deps-next.js") ++ Katex)

  // ------------
  // - Specific -
  // ------------

  val Public = JS("public-deps.js") ++ CSS("public.css")

  val HomeSpa = MemberDeps.all ++ JS("client-home.js")

  val ProjectSpa = MemberDeps addNext JS("client-project.js")

  val Sir = CSS("sir.css")
}
