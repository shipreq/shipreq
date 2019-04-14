package shipreq.webapp.server.app

import japgolly.univeq.UnivEq
import shipreq.webapp.base.{AssetManifest, Urls, WebappConfig}
import shipreq.base.util.{FreeOption, Url}
import shipreq.webapp.server.logic.DispatchLogic

sealed abstract class Endpoint(final val `type`: String, final val name: String)
object Endpoint {

  case object      Comet                                          extends Endpoint("comet", "comet")
  case object      LiftJsStatic                                   extends Endpoint("asset", "lift-js-static")
  case object      LiftJsDynamic                                  extends Endpoint("asset", "lift-js-dynamic")
  case object      LiftAjax                                       extends Endpoint("ajax", "lift-ajax")
  case object      Metrics                                        extends Endpoint("ops", "metrics")
  case object      Unknown                                        extends Endpoint("unknown", "unknown")
  final case class AssetGeneric  (ext: String)                    extends Endpoint("asset", s"asset-$ext")
  final case class AssetSpecific (ext: String, assetName: String) extends Endpoint("asset", s"asset-$ext-$assetName")
  final case class Page          (pageName: String)               extends Endpoint("page", pageName)
  final case class OpsPage       (pageName: String)               extends Endpoint("ops", pageName)
  final case class ServerSideProc(procName: String)               extends Endpoint("ajax", procName)

  private[this] val liftAjaxPrefix      = s"/${WebappConfig.liftPath1}/ajax/"
  private[this] val liftJsDynamicPrefix = s"/${WebappConfig.liftPath1}/page/"
  private[this] val cometPrefix         = s"/${WebappConfig.liftPath1}/comet/"
  private[this] val liftRegex           = "^/[lL]/.*".r.pattern
  private[this] val assetRegex          = "^/.+/[^/.]*\\.([^/]+)$".r
  private[this] val opsPrefix           = DispatchLogic.opsRoot.relativeUrlNoTailSlash + "/"
  private[this] val isSlash             = (_: Char) == '/'

  type Resolver = (String, FreeOption[Endpoint]) => FreeOption[Endpoint]

  def resolver(metricsPath: String, ajaxPaths: Map[Url.Relative, String]): Resolver = {
    val exactMatches = new java.util.HashMap[String, Endpoint]
    for((u, n) <- ajaxPaths) exactMatches.put(u.relativeUrl, ServerSideProc(n))
    exactMatches.put(metricsPath                          , Metrics)
    exactMatches.put(s"/${WebappConfig.liftPath2}/lift.js", LiftJsStatic)
    exactMatches.put(AssetManifest.webappClientHomeJs     , AssetSpecific("js", "shipreq-home"))
    exactMatches.put(AssetManifest.webappClientProjectJs  , AssetSpecific("js", "shipreq-project"))
    exactMatches.put(AssetManifest.webappClientPublicJs   , AssetSpecific("js", "shipreq-public"))
    exactMatches.put(AssetManifest.webappClientWwJs       , AssetSpecific("js", "shipreq-ww"))
    exactMatches.put(AssetManifest.analyticsJs            , AssetSpecific("js", "analytics"))
    exactMatches.put(AssetManifest.loadjs                 , AssetSpecific("js", "load"))
    exactMatches.put(AssetManifest.memberLibBundleJs      , AssetSpecific("js", "member_lib_bundle"))
    exactMatches.put(AssetManifest.vizJs                  , AssetSpecific("js", "viz"))
    exactMatches.put(AssetManifest.semanticJs             , AssetSpecific("js", "semantic"))
    exactMatches.put(AssetManifest.favicon                , AssetSpecific("ico", "favicon"))
    exactMatches.put(AssetManifest.semanticCss            , AssetSpecific("css", "semantic"))
    exactMatches.put(AssetManifest.shipreqBannerSvg       , AssetGeneric("svg"))

    (path, provided) =>
      if (provided.nonEmpty) {
        if (path startsWith opsPrefix)
          provided.getOrNull match {
            case Page(n) => FreeOption(OpsPage(n.dropWhile(isSlash)))
            case _       => provided
          }
        else
          provided
      } else {
        val exact = FreeOption(exactMatches.get(path))
        if (exact.nonEmpty)
          exact
        else if (path startsWith liftJsDynamicPrefix)
          FreeOption(LiftJsDynamic)
        else if (path startsWith liftAjaxPrefix)
          FreeOption(LiftAjax)
        else if (path startsWith cometPrefix)
          FreeOption(Comet)
        else if (liftRegex.matcher(path).matches)
          FreeOption.empty
        else path match {
          case assetRegex(ext) =>
            FreeOption(AssetGeneric(ext))
          case _ =>
            FreeOption.empty
        }
      }
  }

  implicit def univEq: UnivEq[Endpoint] = UnivEq.derive
}
