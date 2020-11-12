package shipreq.webapp.server.config

import shipreq.base.test.BaseTestUtil._
import shipreq.base.util.FreeOption
import shipreq.webapp.server.test.PrepareEnv
import utest._

object EndpointTest extends TestSuite {

  private val metricsPath = "/opsssss/metric"
  private lazy val am = PrepareEnv.global().config.server.assetManifest
  private lazy val sjsm = PrepareEnv.global().config.server.scalaJsManifest
  private lazy val endpoint = Endpoint.resolver(metricsPath, am, sjsm)

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

    "sjs" - {
      "home"      - test(Endpoint.AssetSpecific("js", "shipreq-home"),    sjsm.home)
      "project"   - test(Endpoint.AssetSpecific("js", "shipreq-project"), sjsm.project)
      "public"    - test(Endpoint.AssetSpecific("js", "shipreq-public"),  sjsm.public)
      "webWorker" - test(Endpoint.AssetSpecific("js", "shipreq-ww"),      sjsm.webWorker)
    }

    "specificAssets" - {
      "faviconIco"        - test(Endpoint.AssetSpecific("ico", "favicon"),          am.faviconIco)
      "analyticsJs"       - test(Endpoint.AssetSpecific("js", "analytics"),         am.analyticsJs)
      "loadjs"            - test(Endpoint.AssetSpecific("js", "load"),              am.loadjs)
      "memberLibBundleJs" - test(Endpoint.AssetSpecific("js", "member_lib_bundle"), am.memberLibBundleJs)
      "vizJs"             - test(Endpoint.AssetSpecific("js", "viz"),               am.vizJs)
      "semanticJs"        - test(Endpoint.AssetSpecific("js", "semantic"),          am.semanticJs)
      "semanticCss"       - test(Endpoint.AssetSpecific("css", "semantic"),         am.semanticCss)
    }

    "genericAssets" - {
      "css"   - test(Endpoint.AssetGeneric("css"),    "/blah/x.css")
      "svg"   - test(Endpoint.AssetGeneric("svg"),    "/blah/x.svg")
      "svg"   - test(Endpoint.AssetGeneric("svg"),    "/assets/shipreq-banner.svg")
      "woff2" - test(Endpoint.AssetGeneric("woff2"),  "/blah/x.woff2")
      "woff2" - test(Endpoint.AssetGeneric("woff2"),  "/assets/icons.woff2")
      "jsMap" - test(Endpoint.AssetGeneric("js.map"), "/blah/x.js.map")
      "jsMap" - test(Endpoint.AssetGeneric("js.map"), "/j/webappclientpublic-fastopt.js.map")
    }

    "unknown" - {
      def test(path: String): Unit =
        assertEq(path, endpoint(path, FreeOption.empty).toOption, None)

      "noAssetPath" - test("/blah.js")
    }

  }
}
