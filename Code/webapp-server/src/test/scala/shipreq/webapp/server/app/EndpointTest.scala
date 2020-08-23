package shipreq.webapp.server.app

import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.FreeOption
import shipreq.webapp.base.AssetManifest
import utest._

object EndpointTest extends TestSuite {

  private val metricsPath = "/opsssss/metric"
  private val endpoint = Endpoint.resolver(metricsPath)

  def test(expect: Endpoint, path: String, providedOrNull: Endpoint = null): Unit =
    assertEq(path, endpoint(path, FreeOption(providedOrNull)).toOption, Some(expect))

  override def tests = Tests {

    "securityPolicy" - {
      test(Endpoint.AssetSecurityPolicy, "/L/content-security-policy-report")
    }

    "page" - {
      "root" - test(Endpoint.Page("/"), "/", Endpoint.Page("/"))
      "home" - test(Endpoint.Page("/home"), "/home", Endpoint.Page("/home"))
    }

    "ops" - {
      "ok" - test(Endpoint.OpsPage("ok"), "/ops/ok", Endpoint.Page("/ok"))
      "metrics" - test(Endpoint.Metrics, metricsPath)
    }

    "faviconIco"            - test(Endpoint.AssetSpecific("ico", "favicon"),          AssetManifest.faviconIco)
    "webappClientHomeJs"    - test(Endpoint.AssetSpecific("js", "shipreq-home"),      AssetManifest.webappClientHomeJs)
    "webappClientProjectJs" - test(Endpoint.AssetSpecific("js", "shipreq-project"),   AssetManifest.webappClientProjectJs)
    "webappClientPublicJs"  - test(Endpoint.AssetSpecific("js", "shipreq-public"),    AssetManifest.webappClientPublicJs)
    "webappClientWwJs"      - test(Endpoint.AssetSpecific("js", "shipreq-ww"),        AssetManifest.webappClientWwJs)
    "analyticsJs"           - test(Endpoint.AssetSpecific("js", "analytics"),         AssetManifest.analyticsJs)
    "loadjs"                - test(Endpoint.AssetSpecific("js", "load"),              AssetManifest.loadjs)
    "memberLibBundleJs"     - test(Endpoint.AssetSpecific("js", "member_lib_bundle"), AssetManifest.memberLibBundleJs)
    "vizJs"                 - test(Endpoint.AssetSpecific("js", "viz"),               AssetManifest.vizJs)
    "semanticJs"            - test(Endpoint.AssetSpecific("js", "semantic"),          AssetManifest.semanticJs)
    "semanticCss"           - test(Endpoint.AssetSpecific("css", "semantic"),         AssetManifest.semanticCss)

    "genericAssets" - {
      "css"   - test(Endpoint.AssetGeneric("css"),    "/blah/x.css")
      "svg"   - test(Endpoint.AssetGeneric("svg"),    "/blah/x.svg")
      "svg"   - test(Endpoint.AssetGeneric("svg"),    "/assets/shipreq-banner.svg")
      "woff2" - test(Endpoint.AssetGeneric("woff2"),  "/blah/x.woff2")
      "woff2" - test(Endpoint.AssetGeneric("woff2"),  "/assets/icons.woff2")
      "jsMap" - test(Endpoint.AssetGeneric("js.map"), "/blah/x.js.map")
      "jsMap" - test(Endpoint.AssetGeneric("js.map"), "/j/webapp-client-public-fastopt.js.map")
    }

    "unknown" - {
      def test(path: String): Unit =
        assertEq(path, endpoint(path, FreeOption.empty).toOption, None)

      "noAssetPath" - test("/blah.js")
    }

  }
}
