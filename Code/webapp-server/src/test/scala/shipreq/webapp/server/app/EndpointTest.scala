package shipreq.webapp.server.app

import utest._
import japgolly.microlibs.testutil.TestUtil._
import shipreq.base.util.univeq._
import shipreq.webapp.base.{AssetManifest, WebappConfig}

object EndpointTest extends TestSuite {

  private val metricsPath = "/opsssss/metric"
  private val endpoint = Endpoint.resolver(metricsPath)

  def test(expect: Endpoint, path: String): Unit =
    assertEq(path, endpoint(path).toOption, Some(expect))

  override def tests = TestSuite {

    'comet {
      test(Endpoint.Comet, "/L/comet/11301008944/F86889457609DUVAIE/F86889457707YSS5WS")
      test(Endpoint.Comet, "/L/comet/19408492454/F1135978930148PZUM0K/F1135978930205BEI53F")
      test(Endpoint.Comet, "/L/comet/74023020932/F4523182542865TGETZ/F452318254343CO0NGV")
    }

    'liftJsStatic {
      test(Endpoint.LiftJsStatic, "/l/lift.js")
    }

    'liftJsDynamic {
      test(Endpoint.LiftJsDynamic, "/L/page/F1135978930133VYITVK.js")
      test(Endpoint.LiftJsDynamic, "/L/page/F1958527133110FJNA.js")
    }

    'metrics {
      test(Endpoint.Metrics, metricsPath)
    }

    'webappClientHomeJs    - test(Endpoint.AssetSpecific("ico", "favicon"),          AssetManifest.favicon)
    'webappClientHomeJs    - test(Endpoint.AssetSpecific("js", "shipreq-home"),      AssetManifest.webappClientHomeJs)
    'webappClientProjectJs - test(Endpoint.AssetSpecific("js", "shipreq-project"),   AssetManifest.webappClientProjectJs)
    'webappClientPublicJs  - test(Endpoint.AssetSpecific("js", "shipreq-public"),    AssetManifest.webappClientPublicJs)
    'webappClientWwJs      - test(Endpoint.AssetSpecific("js", "shipreq-ww"),        AssetManifest.webappClientWwJs)
    'analyticsJs           - test(Endpoint.AssetSpecific("js", "analytics"),         AssetManifest.analyticsJs)
    'loadjs                - test(Endpoint.AssetSpecific("js", "load"),              AssetManifest.loadjs)
    'memberLibBundleJs     - test(Endpoint.AssetSpecific("js", "member_lib_bundle"), AssetManifest.memberLibBundleJs)
    'vizJs                 - test(Endpoint.AssetSpecific("js", "viz"),               AssetManifest.vizJs)
    'semanticJs            - test(Endpoint.AssetSpecific("js", "semantic"),          AssetManifest.semanticJs)
    'semanticCss           - test(Endpoint.AssetSpecific("css", "semantic"),         AssetManifest.semanticCss)

    'genericAssets {
      'css   - test(Endpoint.AssetGeneric("css"),    "/x/x.css")
      'svg   - test(Endpoint.AssetGeneric("svg"),    "/x/x.svg")
      'svg   - test(Endpoint.AssetGeneric("svg"),    "/assets/shipreq-banner.svg")
      'woff2 - test(Endpoint.AssetGeneric("woff2"),  "/x/x.woff2")
      'woff2 - test(Endpoint.AssetGeneric("woff2"),  "/assets/icons.woff2")
      'jsMap - test(Endpoint.AssetGeneric("js.map"), "/x/x.js.map")
      'jsMap - test(Endpoint.AssetGeneric("js.map"), "/j/webapp-client-public-fastopt.js.map")
    }

    'unknown {
      def test(path: String): Unit =
        assertEq(path, endpoint(path).toOption, None)

      'noAssetPath - test("/blah.js")
      'unknownLift1 - test(s"/${WebappConfig.liftPath1}/blah.js")
      'unknownLift2 - test(s"/${WebappConfig.liftPath2}/blah.js")
    }

  }
}
