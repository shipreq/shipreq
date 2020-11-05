package shipreq.webapp.server.app

import shipreq.base.util.FreeOption
import shipreq.webapp.base.config.AssetManifest
import shipreq.webapp.base.config.WebappConfig.liftCtxPath
import shipreq.webapp.server.logic.config.ScalaJsManifest
import shipreq.webapp.server.logic.dispatch.DispatchLogic

sealed abstract class Endpoint(final val `type`: String, final val name: String)
object Endpoint {

  case object      AssetSecurityPolicy                            extends Endpoint("asset", "content-security-policy-report")
  case object      Metrics                                        extends Endpoint("ops", "metrics")
  case object      Unknown                                        extends Endpoint("unknown", "unknown")
  final case class AssetGeneric  (ext: String)                    extends Endpoint("asset", s"asset-$ext")
  final case class AssetSpecific (ext: String, assetName: String) extends Endpoint("asset", s"asset-$ext-$assetName")
  final case class Page          (pageName: String)               extends Endpoint("page", pageName)
  final case class OpsPage       (pageName: String)               extends Endpoint("ops", pageName)
  final case class ServerSideProc(procName: String)               extends Endpoint("ajax", procName)

  private[this] val liftRegex  = "^/[lL]/.*".r.pattern
  private[this] val assetRegex = "^/.+/[^/.]*\\.([^/]+)$".r
  private[this] val opsPrefix  = DispatchLogic.opsRoot.relativeUrlNoTailSlash + "/"
  private[this] val isSlash    = (_: Char) == '/'

  type Resolver = (String, FreeOption[Endpoint]) => FreeOption[Endpoint]

  // Note this is only meant to resolve generic requests.
  // Specific requests that DispatchLogic handles correctly set the Endpoint directly via MetricsLogic which results in
  // the FreeOption[Endpoint] param to Resolver being set.
  def resolver(metricsPath: String, am: AssetManifest, sjs: ScalaJsManifest[String]): Resolver = {

    val exactMatches = new java.util.HashMap[String, Endpoint]
    exactMatches.put(metricsPath                                    , Metrics)
    exactMatches.put(s"/$liftCtxPath/content-security-policy-report", AssetSecurityPolicy)
    exactMatches.put(am.analyticsJs                                 , AssetSpecific("js", "analytics"))
    exactMatches.put(am.loadjs                                      , AssetSpecific("js", "load"))
    exactMatches.put(am.memberLibBundleJs                           , AssetSpecific("js", "member_lib_bundle"))
    exactMatches.put(am.vizJs                                       , AssetSpecific("js", "viz"))
    exactMatches.put(am.semanticJs                                  , AssetSpecific("js", "semantic"))
    exactMatches.put(am.semanticCss                                 , AssetSpecific("css", "semantic"))
    exactMatches.put(am.shipreqBannerSvg                            , AssetGeneric("svg"))
    exactMatches.put(am.favicon16X16Png                             , AssetSpecific("png", "favicon"))
    exactMatches.put(am.favicon32X32Png                             , AssetSpecific("png", "favicon"))
    exactMatches.put(am.faviconAndroidChrome192X192Png              , AssetSpecific("png", "favicon"))
    exactMatches.put(am.faviconAndroidChrome512X512Png              , AssetSpecific("png", "favicon"))
    exactMatches.put(am.faviconAppleTouchIconPng                    , AssetSpecific("png", "favicon"))
    exactMatches.put(am.faviconBrowserconfigXml                     , AssetSpecific("xml", "favicon"))
    exactMatches.put(am.faviconIco                                  , AssetSpecific("ico", "favicon"))
    exactMatches.put(am.faviconMstile144X144Png                     , AssetSpecific("png", "favicon"))
    exactMatches.put(am.faviconMstile150X150Png                     , AssetSpecific("png", "favicon"))
    exactMatches.put(am.faviconMstile310X150Png                     , AssetSpecific("png", "favicon"))
    exactMatches.put(am.faviconMstile310X310Png                     , AssetSpecific("png", "favicon"))
    exactMatches.put(am.faviconMstile70X70Png                       , AssetSpecific("png", "favicon"))
    exactMatches.put(am.faviconSafariPinnedTabSvg                   , AssetSpecific("svg", "favicon"))
    exactMatches.put(am.faviconSiteWebmanifest                      , AssetSpecific("webmanifest", "favicon"))

    if (sjs.home     .startsWith("/")) exactMatches.put(sjs.home     , AssetSpecific("js", "shipreq-home"))
    if (sjs.project  .startsWith("/")) exactMatches.put(sjs.project  , AssetSpecific("js", "shipreq-project"))
    if (sjs.public   .startsWith("/")) exactMatches.put(sjs.public   , AssetSpecific("js", "shipreq-public"))
    if (sjs.webWorker.startsWith("/")) exactMatches.put(sjs.webWorker, AssetSpecific("js", "shipreq-ww"))

    (path, provided) => {
      val result =
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
          else if (liftRegex.matcher(path).matches)
            FreeOption.empty
          else path match {
            case assetRegex(ext) =>
              FreeOption(AssetGeneric(ext))
            case _ =>
              FreeOption.empty
          }
        }
      //println(s"[ENDPOINT] $path $provided --> $result")
      result
    }
  }

  implicit def univEq: UnivEq[Endpoint] = UnivEq.derive
}
